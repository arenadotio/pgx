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
  Pga.execute dbh ("DROP DATABASE " ^ db_name) >>| ignore_empty >>| fun () ->
  Log.Global.debug "Dropped database %s" db_name

let create_db dbh ~db_name =
  Pga.execute dbh ("CREATE DATABASE " ^ db_name) >>| ignore_empty
  >>| fun () ->
  Log.Global.debug "Created database %s" db_name

let with_temp_db f =
  Pga.connect ~database:default_database () >>= begin fun dbh ->
    let db_name = random_db () in
    create_db dbh ~db_name >>= fun () ->
    Monitor.protect ~name:db_name (fun () ->
      Pga.connect ~database:db_name () >>= fun test_dbh ->
      Monitor.protect ~name:db_name (fun () -> f test_dbh ~db_name)
        ~finally:(fun () ->
          Pga.close test_dbh >>= fun () ->
          drop_db dbh ~db_name
        )
    ) ~finally:(fun () -> Pga.close dbh)
  end

let with_current_db f =
  Pga.connect () >>= fun dbh ->
  Monitor.protect
    (fun () -> f dbh ~db_name:(Unix.getenv_exn "PGDATABASE"))
    ~finally:(fun () -> Pga.close dbh)

type 'a new_db_callback =
  Pgx_async.t
  -> db_name:string
  -> 'a Deferred.t

let () = Random.self_init ~allow_in_tests:true ()
