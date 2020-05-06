open Base

external reraise : exn -> _ = "%reraise"

module type S = sig
  val run_tests : unit -> unit
end

module type ALCOTEST_IO = sig
  open Alcotest

  type 'a monad
  type 'a test_case

  val test_case : string -> speed_level -> ('a -> unit monad) -> 'a test_case
  val run : string -> (string * unit test_case list) list -> unit
end

module Alcotest_ext = struct
  let uuid = Alcotest.testable Uuidm.pp Uuidm.equal
end

let check_result = Alcotest.(check (list (list (option string))))
let check_results = Alcotest.(check (list (list (list (option string)))))

module Make_tests
    (Pgx_impl : Pgx.S)
    (Alcotest_io : ALCOTEST_IO with type 'a monad := 'a Pgx_impl.Io.t) =
struct
  open Pgx_impl.Io
  open Pgx_impl

  let default_database = "postgres"

  let have_pg_config =
    try
      Unix.getenv "PGUSER" |> ignore;
      true
    with
    | Caml.Not_found | Not_found_s _ -> false
  ;;

  let force_tests =
    try
      (Unix.getenv "PGX_FORCE_TESTS" : string) |> ignore;
      true
    with
    | Caml.Not_found | Not_found_s _ -> false
  ;;

  let set_to_default_db () = Unix.putenv "PGDATABASE" default_database
  let ( >>| ) x f = x >>= fun x -> return (f x)

  type ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

  let try_with f =
    catch (fun () -> f () >>| fun res -> Ok res) (fun e -> return (Error e))
  ;;

  let with_temp_db f =
    let random_db () =
      let random_char () = 10 |> Random.int |> Int.to_string |> fun s -> s.[0] in
      "pgx_test_" ^ String.init 8 ~f:(fun _ -> random_char ())
    in
    let ignore_empty = function
      | [] -> ()
      | _ :: _ -> invalid_arg "ignore_empty"
    in
    let create_db dbh ~db_name =
      execute dbh ("CREATE DATABASE " ^ db_name) >>| ignore_empty
    in
    let drop_db dbh ~db_name =
      execute dbh ("DROP DATABASE " ^ db_name) >>| ignore_empty
    in
    with_conn ~database:default_database (fun dbh ->
        let db_name = random_db () in
        create_db dbh ~db_name
        >>= fun () ->
        connect ~database:db_name ()
        >>= fun test_dbh ->
        protect
          (fun () -> f test_dbh ~db_name)
          ~finally:(fun () -> close test_dbh >>= fun () -> drop_db dbh ~db_name))
  ;;

  let assert_error_test query () =
    try_with (fun () -> with_conn @@ fun dbh -> execute dbh query)
    >>= function
    | Ok _ -> Alcotest.fail "error expected"
    | Error _ -> return ()
  ;;

  let deferred_list_map l ~f =
    List.fold_left
      ~f:(fun acc x -> acc >>= fun acc -> f x >>| fun res -> res :: acc)
      ~init:(return [])
      l
    >>| List.rev
  ;;

  let list_init n f =
    let rec output_list x = if x < n then f x :: output_list (x + 1) else [] in
    output_list 0
  ;;

  let run_tests () =
    Random.self_init ();
    set_to_default_db ();
    let tests =
      [ Alcotest_io.test_case "test db connection" `Quick (fun () ->
            with_temp_db (fun _ ~db_name:_ -> return true)
            >>| Alcotest.(check bool) "with_temp_db makes a connection" true)
      ; Alcotest_io.test_case
          "test fake table"
          `Quick
          (assert_error_test "SELECT * FROM non_exist")
      ; Alcotest_io.test_case "query - 1 query" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "select 1"
                >>| check_results "select 1" [ [ [ Some "1" ] ] ]))
      ; Alcotest_io.test_case "query - multiple" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "select 1; select 2; select 3"
                >>| check_results
                      "select three"
                      [ [ [ Some "1" ] ]; [ [ Some "2" ] ]; [ [ Some "3" ] ] ]))
      ; Alcotest_io.test_case "query - multiple single query" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "select 1 union all select 2 union all select 3"
                >>| check_results
                      "select unit all"
                      [ [ [ Some "1" ]; [ Some "2" ]; [ Some "3" ] ] ]))
      ; Alcotest_io.test_case "query - empty" `Quick (fun () ->
            with_conn (fun dbh -> simple_query dbh "" >>| check_results "empty query" []))
      ; Alcotest_io.test_case
          "test fake column"
          `Quick
          (assert_error_test "SELECT qqq FROM pg_locks")
      ; Alcotest_io.test_case "transaction error recovery" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            try_with (fun () ->
                with_transaction dbh (fun dbh -> simple_query dbh "select * from fake"))
            >>| function
            | Ok _ -> Alcotest.fail "test should fail. table doesn't exist"
            | Error _ -> ())
      ; Alcotest_io.test_case "NoticeResponse in query" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            simple_query dbh "DROP VIEW IF EXISTS fake_view_doesnt_exist"
            >>| List.iter ~f:(check_result "drop view if exists" []))
      ; Alcotest_io.test_case "test fold" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            Prepared.(
              with_prepare dbh ~query:"values (1,2),(3,4)" ~f:(fun s ->
                  execute_fold s ~params:[] ~init:[] ~f:(fun acc a -> return (a :: acc))))
            >>| check_result
                  "fold values"
                  [ [ Some "3"; Some "4" ]; [ Some "1"; Some "2" ] ])
      ; Alcotest_io.test_case "test execute_prepared" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            Prepared.(prepare dbh ~query:"values (1,2),(3,4)" >>= execute ~params:[])
            >>| check_result
                  "prepare & execute"
                  [ [ Some "1"; Some "2" ]; [ Some "3"; Some "4" ] ])
      ; Alcotest_io.test_case "test execute_iter" `Quick (fun () ->
            let n = ref 0 in
            let rows = Array.create ~len:2 [] in
            with_conn
            @@ fun dbh ->
            execute_iter dbh "values (1,2),(3,4)" ~f:(fun row ->
                rows.(!n) <- row;
                n := !n + 1;
                return ())
            >>| fun () ->
            Array.to_list rows
            |> check_result
                 "execute_iter"
                 [ [ Some "1"; Some "2" ]; [ Some "3"; Some "4" ] ])
      ; Alcotest_io.test_case "with_prepare" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let name = "with_prepare" in
            Prepared.(
              with_prepare dbh ~name ~query:"values ($1)" ~f:(fun s ->
                  execute s ~params:[ Some "test" ]))
            >>| check_result name [ [ Some "test" ] ])
      ; Alcotest_io.test_case "interleave unnamed prepares" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let open Prepared in
            with_prepare dbh ~query:"values ($1)" ~f:(fun s1 ->
                with_prepare dbh ~query:"values (1)" ~f:(fun s2 ->
                    execute s1 ~params:[ Some "test" ]
                    >>= fun r1 -> execute s2 ~params:[] >>| fun r2 -> r1, r2))
            >>| fun (r1, r2) ->
            check_result "outer prepare" [ [ Some "test" ] ] r1;
            check_result "inner prepare" [ [ Some "1" ] ] r2)
      ; Alcotest_io.test_case "in_transaction invariant" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            try_with (fun () ->
                with_transaction dbh (fun dbh ->
                    with_transaction dbh (fun _ -> return "unreachable")))
            >>| function
            | Ok "unreachable" -> Alcotest.fail "in_transaction invariant failed"
            | Ok _ -> assert false
            | Error (Invalid_argument _) -> ()
            | Error exn -> reraise exn)
      ; Alcotest_io.test_case "triple prepare no infinite loop" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let name = "triple_prepare" in
            let p () = Prepared.prepare ~name dbh ~query:"values (1,2)" in
            p ()
            >>= fun _ ->
            try_with p
            >>= fun _ ->
            try_with p
            >>| function
            | Ok _ -> Alcotest.fail "Triple prepare should fail"
            | Error (Pgx.PostgreSQL_Error _) -> ()
            | Error exn -> reraise exn)
      ; Alcotest_io.test_case "execute_many function" `Quick (fun () ->
            let params = [ [ Some "1" ]; [ Some "2" ]; [ Some "3" ] ] in
            with_conn (fun dbh ->
                execute_many dbh ~query:"select $1::int" ~params
                >>| check_results
                      "execute_many result"
                      [ [ [ Some "1" ] ]; [ [ Some "2" ] ]; [ [ Some "3" ] ] ]))
      ; Alcotest_io.test_case "query with SET" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "SET LOCAL TIME ZONE 'Europe/Rome'; SELECT 'x'"
                >>| function
                | [ []; [ [ res ] ] ] ->
                  Pgx.Value.to_string_exn res
                  |> Alcotest.(check string) "SELECT after SET" "x"
                | _ -> assert false))
      ; Alcotest_io.test_case "ping" `Quick (fun () -> with_conn (fun dbh -> ping dbh))
      ; Alcotest_io.test_case "with_prepare and describe_statement" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let name = "some name" in
            Prepared.(with_prepare dbh ~name ~query:"values ($1)" ~f:describe)
            >>| fun _ -> ())
      ; Alcotest_io.test_case "should fail without sequencer" `Quick (fun () ->
            with_conn (fun dbh ->
                deferred_list_map
                  (list_init 100 (fun x -> x))
                  ~f:(fun _ -> simple_query dbh "")
                >>| fun _ -> ()))
      ; Alcotest_io.test_case "copy out simple query" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                simple_query
                  dbh
                  "CREATE TABLE tennis_greats ( name            varchar(40), \
                   grand_slams     integer); INSERT INTO tennis_greats VALUES ('Roger \
                   Federer', 19), ('Rafael Nadal', 15); COPY tennis_greats TO STDOUT \
                   (DELIMITER '|')"
                >>| check_results
                      "copy out result"
                      [ []
                      ; []
                      ; [ [ Some "Roger Federer|19\n" ]; [ Some "Rafael Nadal|15\n" ] ]
                      ]))
      ; Alcotest_io.test_case "copy out extended query" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                execute
                  dbh
                  "CREATE TABLE tennis_greats ( name            varchar(40), \
                   grand_slams     integer);"
                >>= fun _ ->
                execute
                  dbh
                  "INSERT INTO tennis_greats VALUES ('Roger Federer', 19), ('Rafael \
                   Nadal', 15);"
                >>= fun _ -> execute dbh "COPY tennis_greats TO STDOUT (DELIMITER '|')")
            >>| check_result
                  "copy out extended result"
                  [ [ Some "Roger Federer|19\n" ]; [ Some "Rafael Nadal|15\n" ] ])
      ; Alcotest_io.test_case "execute_prepared_iter and transact test" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                with_transaction dbh (fun dbh ->
                    execute
                      dbh
                      "CREATE TABLE tennis_greats ( name            varchar(40), \
                       grand_slams     integer);"
                    >>= fun _ ->
                    execute
                      dbh
                      "INSERT INTO tennis_greats VALUES ('Roger Federer', 19), ('Rafael \
                       Nadal', 15);"
                    >>= fun _ ->
                    let open Prepared in
                    with_prepare
                      dbh
                      ~query:
                        "SELECT * FROM tennis_greats WHERE name=$1 AND grand_slams=$2"
                      ~f:(fun s ->
                        let acc = ref [] in
                        execute_iter
                          s
                          ~params:Pgx.Value.[ of_string "Roger Federer"; of_int 19 ]
                          ~f:(fun fields -> return (acc := fields :: !acc))
                        >>= fun () -> return !acc))
                >>| check_result
                      "prepare & transact result"
                      [ [ Some "Roger Federer"; Some "19" ] ]))
      ; Alcotest_io.test_case "commit while not in transaction" `Quick (fun () ->
            try_with (fun () ->
                with_conn
                @@ fun dbh ->
                begin_work dbh >>= fun dbh -> commit dbh >>= fun () -> commit dbh)
            >>= function
            | Ok _ -> Alcotest.fail "commit while not in transaction error expected"
            | Error _ -> return ())
      ; Alcotest_io.test_case "rollback while not in transaction" `Quick (fun () ->
            try_with (fun () ->
                with_conn
                @@ fun dbh ->
                begin_work dbh >>= fun dbh -> commit dbh >>= fun () -> rollback dbh)
            >>= function
            | Ok _ -> Alcotest.fail "rollback while not in transaction error expected"
            | Error _ -> return ())
      ; Alcotest_io.test_case "alive test" `Quick (fun () ->
            with_conn
            @@ fun dbh -> alive dbh >>| Alcotest.(check bool) "alive result" true)
      ; Alcotest_io.test_case "isolation level tests" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                execute
                  dbh
                  "CREATE TABLE tennis_greats ( name            varchar(40), \
                   grand_slams     integer);"
                >>= fun _ ->
                with_transaction ~isolation:Pgx.Isolation.Serializable dbh (fun dbh ->
                    execute dbh "INSERT INTO tennis_greats VALUES ('Roger Federer', 19);")
                >>= fun _ ->
                with_transaction ~isolation:Pgx.Isolation.Repeatable_read dbh (fun dbh ->
                    execute dbh "INSERT INTO tennis_greats VALUES ('Rafael Nadal', 15);")
                >>= fun _ ->
                with_transaction ~isolation:Pgx.Isolation.Read_committed dbh (fun dbh ->
                    execute dbh "INSERT INTO tennis_greats VALUES ('Novak Djokovic', 12);")
                >>= fun _ ->
                with_transaction ~isolation:Pgx.Isolation.Read_uncommitted dbh (fun dbh ->
                    execute dbh "INSERT INTO tennis_greats VALUES ('Andy Murray', 3);")
                >>= fun _ ->
                let open Prepared in
                with_prepare
                  dbh
                  ~query:"SELECT * FROM tennis_greats WHERE name=$1 AND grand_slams=$2"
                  ~f:(fun s ->
                    let acc = ref [] in
                    execute_iter
                      s
                      ~params:[ Some "Andy Murray"; Some "3" ]
                      ~f:(fun fields -> return (acc := fields :: !acc))
                    >>= fun () -> return !acc)
                >>| check_result
                      "isolation query result"
                      [ [ Some "Andy Murray"; Some "3" ] ]))
      ; Alcotest_io.test_case "multi typed table" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                simple_query
                  dbh
                  "CREATE TABLE multi_typed(uuid uuid, int int, string text, numeric \
                   numeric);"
                >>= fun _ ->
                let expect_uuid = Uuidm.create `V4 in
                let all_chars = String.init 255 ~f:Char.of_int_exn in
                let params =
                  let open Pgx.Value in
                  [ of_uuid expect_uuid
                  ; of_int 12
                  ; of_string all_chars
                  ; of_string "9223372036854775807"
                  ]
                in
                execute
                  dbh
                  ~params
                  "INSERT INTO multi_typed (uuid, int, string, numeric) VALUES ($1, $2, \
                   $3, $4)"
                >>= fun _ ->
                simple_query dbh "SELECT * FROM multi_typed"
                >>| function
                | [ [ [ uuid; int_; string_; numeric ] ] ] ->
                  let open Pgx.Value in
                  let uuid = to_uuid uuid in
                  let int_ = to_int int_ in
                  let string_ = to_string string_ in
                  let numeric = to_string numeric in
                  Alcotest.(Alcotest_ext.(check (option uuid)))
                    "uuid"
                    (Some expect_uuid)
                    uuid;
                  Alcotest.(check (option int)) "int" (Some 12) int_;
                  Alcotest.(check (option string)) "string" (Some all_chars) string_;
                  Alcotest.(check (option string))
                    "numeric"
                    (Some "9223372036854775807")
                    numeric
                | _ ->
                  Alcotest.fail "Error: multi typed table: got unexpected query result"))
      ; Alcotest_io.test_case "binary string handling" `Quick (fun () ->
            let all_chars = String.init 255 ~f:Char.of_int_exn in
            with_conn (fun db ->
                [ "SELECT decode($1, 'base64')", Base64.encode_exn all_chars, all_chars
                  (* Postgres adds whitespace to base64 encodings, so we strip it
                     back out *)
                ; ( "SELECT regexp_replace(encode($1, 'base64'), '\\s', '', 'g')"
                  , all_chars
                  , Base64.encode_exn all_chars )
                ]
                |> deferred_list_map ~f:(fun (query, param, expect) ->
                       let params = [ param |> Pgx.Value.of_string ] in
                       execute ~params db query
                       >>| function
                       | [ [ Some actual ] ] ->
                         Alcotest.(check string) "binary string" expect actual
                       | _ -> assert false))
            >>| List.iter ~f:(fun () -> ()))
      ; Alcotest_io.test_case "UTF-8 partial round-trip 1" `Quick (fun () ->
            (* Select a literal string *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                simple_query
                  db
                  {|
                CREATE TEMPORARY TABLE this_test (id text);
                INSERT INTO this_test (id) VALUES ('test-ä-test')
              |}
                >>= fun _ ->
                execute db "SELECT id FROM this_test"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string option]
                    ~expect:(Some expect)
                    (Pgx.Value.to_string result)
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 partial round-trip 1 with where" `Quick (fun () ->
            (* Select a literal string *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                simple_query
                  db
                  {|
                  CREATE TEMPORARY TABLE this_test (id text);
                  INSERT INTO this_test (id) VALUES ('test-ä-test')
                |}
                >>= fun _ ->
                execute
                  db
                  ~params:[ Pgx.Value.of_string expect ]
                  "SELECT id FROM this_test WHERE id = $1"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string option]
                    ~expect:(Some expect)
                    (Pgx.Value.to_string result)
                | [] -> Alcotest.fail "Expected one row but got zero"
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 partial round-trip 2" `Quick (fun () ->
            (* Insert string as a param, then select back the contents of
               the table *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                simple_query db "CREATE TEMPORARY TABLE this_test (id text)"
                >>= fun _ ->
                execute
                  db
                  ~params:[ Pgx.Value.of_string expect ]
                  "INSERT INTO this_test (id) VALUES ($1)"
                >>= fun _ ->
                execute db "SELECT id FROM this_test"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string option]
                    ~expect:(Some expect)
                    (Pgx.Value.to_string result)
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 partial round-trip 3" `Quick (fun () ->
            with_conn (fun db ->
                simple_query
                  db
                  {|
                    CREATE TEMPORARY TABLE this_test (id text);
                    INSERT INTO this_test (id) VALUES('test-\303\244-test')
                  |}
                >>= fun _ ->
                execute db "SELECT id FROM this_test"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string]
                    ~expect:{|test-\303\244-test|}
                    (Pgx.Value.to_string_exn result)
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 round-trip" `Quick (fun () ->
            (* Select the contents of a param *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                execute db ~params:[ Pgx.Value.of_string expect ] "SELECT $1::VARCHAR"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string option]
                    ~expect:(Some expect)
                    (Pgx.Value.to_string result)
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 round-trip where" `Quick (fun () ->
            (* Insert string as a param, then select back the contents of
               the table using a WHERE *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                simple_query db "CREATE TEMPORARY TABLE this_test (id text)"
                >>= fun _ ->
                execute
                  db
                  ~params:[ Pgx.Value.of_string expect ]
                  "INSERT INTO this_test (id) VALUES ($1)"
                >>= fun _ ->
                execute
                  db
                  ~params:[ Pgx.Value.of_string expect ]
                  "SELECT id FROM this_test WHERE id = $1"
                >>| function
                | [ [ result ] ] ->
                  [%test_result: string option]
                    ~expect:(Some expect)
                    (Pgx.Value.to_string result)
                | _ -> assert false))
      ]
    in
    if force_tests || have_pg_config
    then Alcotest_io.run "pgx_test" [ "pgx_async", tests ]
    else Caml.print_endline "Skipping PostgreSQL tests since PGUSER is unset."
  ;;
end
