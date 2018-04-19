open Core
open Async

module Pga = Pgx_async

let default_database = "postgres"

let set_to_default_db () = Unix.putenv ~key:"PGDATABASE" ~data:default_database

let random_db () =
  let random_char () = 10 |> Random.int |> Int.to_string |> Char.of_string in
  "pgx_test_" ^ String.init 8 ~f:(fun _ -> random_char ())

let ignore_empty = function
  | [] -> ()
  | _::_ -> invalid_arg "ignore_empty"

let drop_db dbh ~db_name =
  Pga.execute dbh ("DROP DATABASE " ^ db_name)
  >>| ignore_empty

let create_db dbh ~db_name =
  Pga.execute dbh ("CREATE DATABASE " ^ db_name)
  >>| ignore_empty

let with_temp_db f =
  let db_name = random_db () in
  Pga.with_conn ~database:default_database (fun dbh ->
    create_db dbh ~db_name
    >>= fun () ->
    Monitor.protect (fun () ->
      Pga.with_conn ~database:db_name (fun test_dbh ->
        f test_dbh ~db_name))
      ~finally:(fun () ->
        drop_db dbh ~db_name))

type 'a new_db_callback =
  Pgx_async.t
  -> db_name:string
  -> 'a Deferred.t

let () = Random.self_init ~allow_in_tests:true ()
