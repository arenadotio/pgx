open OUnit2

module type S = sig
  type 'a monad
  val run_tests : unit -> unit monad
end

module Make_tests (IO : Pgx.IO) = struct
  module Pgx_impl = Pgx.Make (IO)

  open IO
  open Pgx_impl

  type 'a monad = 'a IO.t

  let default_database = "postgres"

  type async_test = unit -> unit IO.t

  let set_to_default_db () =
    Unix.putenv "PGDATABASE" default_database

  let (>>|) x f = x >>= fun x -> return (f x)

  type ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

  let try_with f =
    catch
      (fun () -> f () >>| fun res -> Ok res)
      (fun e -> return (Error e))

  let with_temp_db f =
    let random_db () =
      let random_char () =
        10
        |> Random.int
        |> string_of_int
        |> fun s -> s.[0]
      in
      "pgx_test_" ^ String.init 8 (fun _ -> random_char ()) in
    let ignore_empty = function
      | [] -> ()
      | _::_ -> invalid_arg "ignore_empty" in
    let create_db dbh ~db_name =
      execute dbh ("CREATE DATABASE " ^ db_name) >>|
      ignore_empty >>= fun () ->
      debug ("Created database " ^ db_name) in
    let drop_db dbh ~db_name =
      execute dbh ("DROP DATABASE " ^ db_name) >>|
      ignore_empty >>= fun () ->
      debug ("Dropped database " ^ db_name) in
    connect ~database:default_database () >>= begin fun dbh ->
      let db_name = random_db () in
      create_db dbh ~db_name >>= fun () ->
      protect (fun () ->
        connect ~database:db_name () >>= fun test_dbh ->
        protect (fun () -> f test_dbh ~db_name)
          ~finally:(fun () ->
            close test_dbh >>= fun () ->
            drop_db dbh ~db_name
          )
      ) ~finally:(fun () -> close dbh)
    end

  let assert_error_test query () =
    try_with (fun () ->
      with_conn @@ fun dbh ->
      execute dbh query) >>= function
    | Ok _ -> failwith "error expected"
    | Error _ -> return ()

  let deferred_list_map l ~f =
    List.fold_left (fun acc x ->
      acc
      >>= fun acc ->
      f x
      >>| fun res ->
      res :: acc)
      (return []) l
    >>| List.rev

  let make_tests suite_name tests =
    deferred_list_map tests
      ~f: (fun ((name, test) : (string * async_test)) ->
        debug ("Running " ^ name)
        >>= fun () ->
        try_with test
        >>| function
        | Ok () -> (name, `Ok)
        | Error exn -> (name, `Exn exn))
    >>| List.map (fun (name, res) ->
      name >:: fun _ ->
        match res with
        | `Ok -> ()
        | `Exn x -> (raise x))
    >>| (>:::) suite_name

  let list_init n f =
    let rec output_list x =
      if x < n
      then (f x) :: (output_list (x+1))
      else [] in
    output_list 0

  let string_list_to_string sl =
    let str = String.concat ";" sl in
    "[" ^ str ^ "]"

  let pretty_print_string_option_list_list l =
    (List.map
       (fun sol ->
          let sl = List.map (function
            | None -> "None"
            | Some s -> "Some \"" ^ s ^ "\"") sol in
          string_list_to_string sl)
       l)
    |> string_list_to_string

  let pretty_print_string_option_list_list_list l =
    List.map pretty_print_string_option_list_list l
    |> string_list_to_string

  let run_tests () =
    Random.self_init ();
    set_to_default_db ();
    let tests =
      ["test db connection", (fun () ->
         with_temp_db (fun _ ~db_name:_ -> return true)
         >>| assert_bool "with_temp_db makes a connection"
       )
      ; "test fake table", (assert_error_test "SELECT * FROM non_exist")
      ; "query - 1 query", (fun () ->
          with_conn (fun dbh ->
            simple_query dbh "select 1" >>|
            assert_equal [[[Some "1"]]])
        )
      ; "query - multiple", (fun () ->
          with_conn (fun dbh ->
            simple_query dbh "select 1; select 2; select 3" >>|
            assert_equal
              [ [[Some "1"]]
              ; [[Some "2"]]
              ; [[Some "3"]]] )
        )
      ; "query - multiple single query", (fun () ->
          with_conn (fun dbh ->
            simple_query dbh "select 1 union all select 2 union all select 3"
            >>| assert_equal
            ~printer:pretty_print_string_option_list_list_list
              [[ [Some "1"]
               ; [Some "2"]
               ; [Some "3"]]] )
        )
      ; "query - empty", (fun () ->
          with_conn (fun dbh ->
            simple_query dbh "" >>|
            assert_equal [])
        )
      ; "test fake column", (assert_error_test "SELECT qqq FROM pg_locks")
      ; "transaction error recovery", (fun () ->
          with_conn @@ fun dbh ->
          try_with (fun () ->
            with_transaction dbh (fun dbh ->
              simple_query dbh "select * from fake"))
          >>| function
          | Ok _ -> assert_failure "test should fail. table doesn't exist"
          | Error _ -> ()
        )
      ; "NoticeResponse in query", (fun () ->
          with_conn @@ fun dbh ->
          simple_query dbh "DROP VIEW IF EXISTS fake_view_doesnt_exist"
          >>| List.iter (assert_equal [])
        )
      ; "test fold", (fun () ->
          with_conn @@ fun dbh ->
          Prepared.(with_prepare dbh ~query:"values (1,2),(3,4)" ~f:(fun s ->
            execute_fold s ~params:[] ~init:[] ~f:(fun acc a ->
              return (a :: acc))))
          >>| assert_equal
                [ [Some "3"; Some "4"]
                ; [Some "1"; Some "2"] ]
        )
      ; "test execute_prepared", (fun () ->
          with_conn @@ fun dbh ->
          Prepared.(prepare dbh ~query:"values (1,2),(3,4)"
                    >>= execute ~params:[])
          >>| assert_equal
                [ [Some "1"; Some "2"]
                ; [Some "3"; Some "4"] ]
        )
      ; "test execute_iter", (fun () ->
          let n = ref 0 in
          let rows = Array.make 2 [] in
          with_conn @@ fun dbh ->
          execute_iter dbh "values (1,2),(3,4)" ~f:(fun row ->
            Array.set rows !n row;
            n := !n + 1;
            return ())
          >>| fun () ->
          Array.to_list rows
          |> assert_equal [ [Some "1"; Some "2"]
                          ; [Some "3"; Some "4"] ]
        )
      ; "with_prepare", (fun () ->
          with_conn @@ fun dbh ->
          let name = "with_prepare" in
          Prepared.(with_prepare dbh ~name ~query:"values ($1)" ~f:(fun s ->
            execute s ~params:[Some "test"]))
          >>| assert_equal [[Some "test"]]
        )
      ; "interleave unnamed prepares", (fun () ->
          with_conn @@ fun dbh ->
          let open Prepared in
          with_prepare dbh ~query:"values ($1)" ~f:(fun s1 ->
            with_prepare dbh ~query:"values (1)" ~f:(fun s2 ->
              execute s1 ~params:[Some "test"]
              >>= fun r1 ->
              execute s2 ~params:[]
              >>| fun r2 ->
              r1, r2))
          >>| assert_equal ([[Some "test"]], [[ Some "1" ]])
        )
      ; "in_transaction invariant", (fun () ->
          with_conn @@ fun dbh ->
          try_with (fun () ->
            with_transaction dbh (fun dbh ->
              with_transaction dbh (fun _ -> return "unreachable"))) >>| function
          | Ok "unreachable" -> failwith "in_transaction invariant failed"
          | Ok _ -> assert false
          | Error (Invalid_argument _) -> ()
          | Error exn -> raise exn
        )
      ; "triple prepare no infinite loop", (fun () ->
          with_conn @@ fun dbh ->
          let name = "triple_prepare" in
          let p () =
            Prepared.prepare ~name dbh ~query:"values (1,2)" in
          p () >>= fun _ ->
          try_with p
          >>= fun _ ->
          try_with p
          >>| function
          | Ok _ -> failwith "Triple prepare should fail"
          | Error (Pgx.PostgreSQL_Error _) -> ()
          | Error exn -> raise exn
        )
      ; "execute_many function", (fun () ->
          let params = [[ Some "1" ] ; [ Some "2" ] ; [ Some "3"]] in
          with_conn (fun dbh ->
            execute_many dbh ~query:"select $1::int" ~params >>|
            assert_equal
              [ [[ Some "1"]]
              ; [[ Some "2"]]
              ; [[ Some "3"]] ])
        )
      ; "query with SET", (fun () ->
          with_conn (fun dbh ->
            simple_query dbh "SET LOCAL TIME ZONE 'Europe/Rome'; \
                              SELECT 'x'"
            >>| function
            | [[]; [[ res ]]] ->
              Pgx.Value.to_string_exn res
              |> assert_equal ~printer:(fun x -> x) "x"
            | _ -> assert false)
        )
      ; "ping", (fun () ->
          with_conn (fun dbh ->
            ping dbh)
        )
      ; "with_prepare and describe_statement", (fun () ->
          with_conn @@ fun dbh ->
          let name = "some name" in
          Prepared.(with_prepare dbh ~name ~query:"values ($1)"
                      ~f:describe)
          >>| fun _ -> ())
      ; "should fail without sequencer", (fun () ->
          with_conn (fun dbh ->
            deferred_list_map (list_init 100 (fun x -> x)) ~f:(fun _ ->
              simple_query dbh "") >>| fun _ -> ())
        )
      ; "copy out simple query", (fun () ->
          with_temp_db (fun dbh ~db_name:_ ->
            simple_query dbh
              ("CREATE TABLE tennis_greats ( \
                name            varchar(40), \
                grand_slams     integer); \
                INSERT INTO tennis_greats VALUES \
                ('Roger Federer', 19), \
                ('Rafael Nadal', 15); \
                COPY tennis_greats TO STDOUT (DELIMITER '|')")
            >>| assert_equal [[];[];[[Some "Roger Federer|19\n"];[Some "Rafael Nadal|15\n"]];[]])
        )
      ; "copy out extended query", (fun () ->
          with_temp_db (fun dbh ~db_name:_ ->
            execute dbh
              "CREATE TABLE tennis_greats ( \
               name            varchar(40), \
               grand_slams     integer);"
            >>= fun _ ->
            execute dbh "INSERT INTO tennis_greats VALUES \
                         ('Roger Federer', 19), \
                         ('Rafael Nadal', 15);"
            >>= fun _ ->
            execute dbh "COPY tennis_greats TO STDOUT (DELIMITER '|')")
          >>| assert_equal [[Some "Roger Federer|19\n"];[Some "Rafael Nadal|15\n"]]
        )
      ; "execute_prepared_iter and transact test", (fun () ->
          with_temp_db (fun dbh ~db_name:_ ->
            with_transaction dbh (fun dbh ->
              execute dbh
                "CREATE TABLE tennis_greats ( \
                 name            varchar(40), \
                 grand_slams     integer);"
              >>= fun _ ->
              execute dbh "INSERT INTO tennis_greats VALUES \
                           ('Roger Federer', 19), \
                           ('Rafael Nadal', 15);"
              >>= fun _ ->
              let open Prepared in
              with_prepare dbh
                ~query:"SELECT * FROM tennis_greats \
                        WHERE name=$1 AND grand_slams=$2"
                ~f:(fun s ->
                  let acc = ref [] in
                  execute_iter s
                    ~params:Pgx.Value.([of_string "Roger Federer"; of_int 19])
                    ~f:(fun fields -> return (acc := fields::!acc))
                  >>= fun () -> return (!acc)))
            >>| assert_equal [[Some "Roger Federer";Some "19"]]
          )
        )
      ; "commit while not in transaction", (fun () ->
          try_with (fun () ->
            with_conn @@ fun dbh ->
            begin_work dbh
            >>= fun dbh ->
            commit dbh
            >>= fun () ->
            commit dbh) >>= function
          | Ok _ -> failwith "commit while not in transaction \
                              error expected"
          | Error _ -> return ()
        )
      ; "rollback while not in transaction", (fun () ->
          try_with (fun () ->
            with_conn @@ fun dbh ->
            begin_work dbh
            >>= fun dbh ->
            commit dbh
            >>= fun () ->
            rollback dbh) >>= function
          | Ok _ -> failwith "rollback while not in transaction \
                              error expected"
          | Error _ -> return ()
        )
      ; "alive test", (fun () ->
          with_conn @@ fun dbh ->
          alive dbh
          >>| assert_equal true
        )
      ; "isolation level tests", (fun () ->
          with_temp_db (fun dbh ~db_name:_ ->
            execute dbh
              "CREATE TABLE tennis_greats ( \
               name            varchar(40), \
               grand_slams     integer);"
            >>= fun _ ->
            with_transaction ~isolation:Pgx.Isolation.Serializable dbh (fun dbh ->
              execute dbh "INSERT INTO tennis_greats VALUES \
                           ('Roger Federer', 19);"
            )
            >>= fun _ ->
            with_transaction ~isolation:Pgx.Isolation.Repeatable_read dbh (fun dbh ->
              execute dbh "INSERT INTO tennis_greats VALUES \
                           ('Rafael Nadal', 15);"
            )
            >>= fun _ ->
            with_transaction ~isolation:Pgx.Isolation.Read_committed dbh (fun dbh ->
              execute dbh "INSERT INTO tennis_greats VALUES \
                           ('Novak Djokovic', 12);"
            )
            >>= fun _ ->
            with_transaction ~isolation:Pgx.Isolation.Read_uncommitted dbh (fun dbh ->
              execute dbh "INSERT INTO tennis_greats VALUES \
                           ('Andy Murray', 3);"
            )
            >>= fun _ ->
            let open Prepared in
            with_prepare dbh
              ~query:"SELECT * FROM tennis_greats \
                      WHERE name=$1 AND grand_slams=$2"
              ~f:(fun s ->
                let acc = ref [] in
                execute_iter s ~params:[Some "Andy Murray"; Some "3"]
                  ~f:(fun fields -> return (acc := fields::!acc))
                >>= fun () -> return (!acc))
            >>| assert_equal [[Some "Andy Murray";Some "3"]]
          )
        )
      ; "multi typed table", (fun () ->
          with_temp_db (fun dbh ~db_name:_ ->
            simple_query dbh ("CREATE TABLE multi_typed\
                               (uuid uuid, \
                               int int, \
                               string text, \
                               numeric numeric);")
            >>= fun _ ->
            let expect_uuid = Uuidm.create `V4 in
            let all_chars = String.init 255 char_of_int in
            let params =
              let open Pgx.Value in
              [ of_uuid expect_uuid
              ; of_int 12
              ; of_string all_chars
              ; of_string "9223372036854775807" ] in
            execute dbh ~params "INSERT INTO multi_typed (uuid, int, \
                                 string, numeric) VALUES ($1, $2, $3, $4)"
            >>= fun _ ->
            simple_query dbh "SELECT * FROM multi_typed"
            >>| function
            | [[[ uuid; int_; string_ ; numeric ]]] ->
              let open Pgx.Value in
              let uuid = to_uuid uuid in
              let int_ = to_int int_ in
              let string_ = to_string string_ in
              let numeric = to_string numeric in
              assert_equal (Some expect_uuid) uuid;
              assert_equal (Some 12) int_;
              assert_equal (Some all_chars)
                ~printer:(function Some v -> v | None -> "(None)") string_;
              assert_equal (Some "9223372036854775807") numeric;
            | _ -> failwith "Error: multi typed table: got unexpected query result"
          ))
      ; "binary string handling", (fun () ->
          let all_chars = String.init 255 char_of_int in
          with_conn (fun db ->
            [ "SELECT decode($1, 'base64')", B64.encode all_chars, all_chars
            (* Postgres adds whitespace to base64 encodings, so we strip it
               back out *)
            ; "SELECT regexp_replace(encode($1, 'base64'), '\\s', '', 'g')",
              all_chars, B64.encode all_chars ]
            |> deferred_list_map ~f:(fun (query, param, expect) ->
              let params = [ param |> Pgx.Value.of_string ] in
              execute ~params db query
              >>| function
              | [[ Some actual ]] ->
                assert_equal ~printer:(fun x -> Printf.sprintf "'%s'" x) expect actual
              | _ -> assert false))
          >>| List.iter (fun () -> ())
        )
      ] in
    make_tests "pgx_async" tests
    >>| run_test_tt_main ~exit
end
