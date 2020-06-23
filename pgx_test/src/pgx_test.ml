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

  let pgx_value =
    Alcotest.testable
      (fun fmt t ->
        Pgx.Value.sexp_of_t t |> Sexplib0.Sexp.to_string_hum |> Format.pp_print_string fmt)
      (fun a b -> Pgx.Value.compare a b = 0)
  ;;
end

let check_result = Alcotest.(check (list (list Alcotest_ext.pgx_value)))
let check_results = Alcotest.(check (list (list (list Alcotest_ext.pgx_value))))

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
    | Not_found -> false
  ;;

  let force_tests =
    try
      (Unix.getenv "PGX_FORCE_TESTS" : string) |> ignore;
      true
    with
    | Not_found -> false
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
      let random_char () = 10 |> Random.int |> string_of_int |> fun s -> s.[0] in
      "pgx_test_" ^ String.init 8 (fun _ -> random_char ())
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
    | Ok _ -> failwith "error expected"
    | Error _ -> return ()
  ;;

  let deferred_list_map l ~f =
    List.fold_left
      (fun acc x -> acc >>= fun acc -> f x >>| fun res -> res :: acc)
      (return [])
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
                >>| check_results "select 1" [ [ [ Pgx.Value.of_string "1" ] ] ]))
      ; Alcotest_io.test_case "query - multiple" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "select 1; select 2; select 3"
                >>| check_results
                      "select three"
                      Pgx.Value.
                        [ [ [ of_string "1" ] ]
                        ; [ [ of_string "2" ] ]
                        ; [ [ of_string "3" ] ]
                        ]))
      ; Alcotest_io.test_case "query - multiple single query" `Quick (fun () ->
            with_conn (fun dbh ->
                simple_query dbh "select 1 union all select 2 union all select 3"
                >>| check_results
                      "select unit all"
                      Pgx.Value.
                        [ [ [ of_string "1" ]; [ of_string "2" ]; [ of_string "3" ] ] ]))
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
            >>| List.iter (check_result "drop view if exists" []))
      ; Alcotest_io.test_case "test fold" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            Prepared.(
              with_prepare dbh ~query:"values (1,2),(3,4)" ~f:(fun s ->
                  execute_fold s ~params:[] ~init:[] ~f:(fun acc a -> return (a :: acc))))
            >>| check_result
                  "fold values"
                  Pgx.Value.
                    [ [ of_string "3"; of_string "4" ]; [ of_string "1"; of_string "2" ] ])
      ; Alcotest_io.test_case "test execute_prepared" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            Prepared.(prepare dbh ~query:"values (1,2),(3,4)" >>= execute ~params:[])
            >>| check_result
                  "prepare & execute"
                  Pgx.Value.
                    [ [ of_string "1"; of_string "2" ]; [ of_string "3"; of_string "4" ] ])
      ; Alcotest_io.test_case "test execute_iter" `Quick (fun () ->
            let n = ref 0 in
            let rows = Array.make 2 [] in
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
                 Pgx.Value.
                   [ [ of_string "1"; of_string "2" ]; [ of_string "3"; of_string "4" ] ])
      ; Alcotest_io.test_case "with_prepare" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let name = "with_prepare" in
            Prepared.(
              with_prepare dbh ~name ~query:"values ($1)" ~f:(fun s ->
                  execute s ~params:Pgx.Value.[ of_string "test" ]))
            >>| check_result name Pgx.Value.[ [ of_string "test" ] ])
      ; Alcotest_io.test_case "interleave unnamed prepares" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            let open Prepared in
            with_prepare dbh ~query:"values ($1)" ~f:(fun s1 ->
                with_prepare dbh ~query:"values (1)" ~f:(fun s2 ->
                    execute s1 ~params:Pgx.Value.[ of_string "test" ]
                    >>= fun r1 -> execute s2 ~params:[] >>| fun r2 -> r1, r2))
            >>| fun (r1, r2) ->
            check_result "outer prepare" Pgx.Value.[ [ of_string "test" ] ] r1;
            check_result "inner prepare" Pgx.Value.[ [ of_string "1" ] ] r2)
      ; Alcotest_io.test_case "in_transaction invariant" `Quick (fun () ->
            with_conn
            @@ fun dbh ->
            try_with (fun () ->
                with_transaction dbh (fun dbh ->
                    with_transaction dbh (fun _ -> return "unreachable")))
            >>| function
            | Ok "unreachable" -> failwith "in_transaction invariant failed"
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
            | Ok _ -> failwith "Triple prepare should fail"
            | Error (Pgx.PostgreSQL_Error _) -> ()
            | Error exn -> reraise exn)
      ; Alcotest_io.test_case "execute_many function" `Quick (fun () ->
            let params =
              Pgx.Value.[ [ of_string "1" ]; [ of_string "2" ]; [ of_string "3" ] ]
            in
            with_conn (fun dbh ->
                execute_many dbh ~query:"select $1::int" ~params
                >>| check_results
                      "execute_many result"
                      Pgx.Value.
                        [ [ [ of_string "1" ] ]
                        ; [ [ of_string "2" ] ]
                        ; [ [ of_string "3" ] ]
                        ]))
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
                      Pgx.Value.
                        [ []
                        ; []
                        ; [ [ of_string "Roger Federer|19\n" ]
                          ; [ of_string "Rafael Nadal|15\n" ]
                          ]
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
                  Pgx.Value.
                    [ [ of_string "Roger Federer|19\n" ]
                    ; [ of_string "Rafael Nadal|15\n" ]
                    ])
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
                      Pgx.Value.[ [ of_string "Roger Federer"; of_string "19" ] ]))
      ; Alcotest_io.test_case "commit while not in transaction" `Quick (fun () ->
            try_with (fun () ->
                with_conn
                @@ fun dbh ->
                begin_work dbh >>= fun dbh -> commit dbh >>= fun () -> commit dbh)
            >>= function
            | Ok _ -> failwith "commit while not in transaction error expected"
            | Error _ -> return ())
      ; Alcotest_io.test_case "rollback while not in transaction" `Quick (fun () ->
            try_with (fun () ->
                with_conn
                @@ fun dbh ->
                begin_work dbh >>= fun dbh -> commit dbh >>= fun () -> rollback dbh)
            >>= function
            | Ok _ -> failwith "rollback while not in transaction error expected"
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
                      ~params:Pgx.Value.[ of_string "Andy Murray"; of_string "3" ]
                      ~f:(fun fields -> return (acc := fields :: !acc))
                    >>= fun () -> return !acc)
                >>| check_result
                      "isolation query result"
                      Pgx.Value.[ [ of_string "Andy Murray"; of_string "3" ] ]))
      ; Alcotest_io.test_case "multi typed table" `Quick (fun () ->
            with_temp_db (fun dbh ~db_name:_ ->
                simple_query
                  dbh
                  "CREATE TABLE multi_typed(uuid uuid, int int, string text, numeric \
                   numeric);"
                >>= fun _ ->
                let expect_uuid = Uuidm.create `V4 in
                let params =
                  let open Pgx.Value in
                  [ of_uuid expect_uuid
                  ; of_int 12
                  ; of_string "asdf"
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
                  Alcotest.(check (option string)) "string" (Some "asdf") string_;
                  Alcotest.(check (option string))
                    "numeric"
                    (Some "9223372036854775807")
                    numeric
                | _ ->
                  Alcotest.fail "Error: multi typed table: got unexpected query result"))
      ; Alcotest_io.test_case "binary string handling" `Quick (fun () ->
            let all_chars = String.init 255 char_of_int in
            with_conn (fun db ->
                [ ( "SELECT decode($1, 'base64')::bytea"
                  , Base64.encode_exn all_chars |> Pgx.Value.of_string
                  , Pgx.Value.to_binary_exn
                  , all_chars )
                  (* Postgres adds whitespace to base64 encodings, so we strip it
                     back out *)
                ; ( "SELECT regexp_replace(encode($1::bytea, 'base64'), '\\s', '', 'g')"
                  , Pgx.Value.of_binary all_chars
                  , Pgx.Value.to_string_exn
                  , Base64.encode_exn all_chars )
                ]
                |> deferred_list_map ~f:(fun (query, param, read_f, expect) ->
                       let params = [ param ] in
                       execute ~params db query
                       >>| function
                       | [ [ actual ] ] ->
                         read_f actual |> Alcotest.(check string) "binary string" expect
                       | _ -> assert false))
            >>| List.iter (fun () -> ()))
      ; Alcotest_io.test_case "binary string round-trip" `Quick (fun () ->
            let all_chars = String.init 255 char_of_int in
            with_conn (fun db ->
                (* This binary string should get encoded as hex and stored as one byte-per-byte of input *)
                let params = [ Pgx.Value.of_binary all_chars ] in
                (* Checking here that Postgres doesn't throw an exception about null characters in input, since
                   our encoded input has no null chars *)
                execute ~params db "SELECT $1::bytea, octet_length($1::bytea)"
                >>| function
                | [ [ value; length ] ] ->
                  Pgx.Value.to_binary_exn value
                  |> Alcotest.(check string) "binary string contents" all_chars;
                  (* Our string is 255 bytes so it should be stored as 255 bytes, not as 512 (the length of the
                     encoded hex). What we're testing here is that we're actually storing binary, not hex
                     encoded binary *)
                  Pgx.Value.to_int_exn length
                  |> Alcotest.(check int) "binary string length" 255
                | _ -> assert false))
      ; Alcotest_io.test_case "Non-binary literal hex string round-trip" `Quick (fun () ->
            with_conn (fun db ->
                (* This hex string should get inserted into the DB as literally "\x0001etc" *)
                let input =
                  "\\x000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfe"
                in
                let params = [ Pgx.Value.of_string input ] in
                execute ~params db "SELECT $1::varchar, octet_length($1::varchar)"
                >>| function
                | [ [ value; length ] ] ->
                  Pgx.Value.to_string_exn value
                  |> Alcotest.(check string) "string contents" input;
                  Pgx.Value.to_int_exn length |> Alcotest.(check int) "string length" 512
                | _ -> assert false))
      ; Alcotest_io.test_case "Binary literal hex string round-trip" `Quick (fun () ->
            with_conn (fun db ->
                (* This hex string should get double encoded so it makes it into the DB as literally "\x0001etc" *)
                let input =
                  "\\x000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfe"
                in
                let params = [ Pgx.Value.of_binary input ] in
                execute ~params db "SELECT $1::bytea, octet_length($1::bytea)"
                >>| function
                | [ [ value; length ] ] ->
                  Pgx.Value.to_binary_exn value
                  |> Alcotest.(check string) "string contents" input;
                  Pgx.Value.to_int_exn length |> Alcotest.(check int) "string length" 512
                | _ -> assert false))
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
                  Alcotest.(check (option string))
                    ""
                    (Some expect)
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
                  Alcotest.(check (option string))
                    ""
                    (Some expect)
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
                  Alcotest.(check (option string))
                    ""
                    (Some expect)
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
                  Alcotest.(check string)
                    ""
                    {|test-\303\244-test|}
                    (Pgx.Value.to_string_exn result)
                | _ -> assert false))
      ; Alcotest_io.test_case "UTF-8 round-trip" `Quick (fun () ->
            (* Select the contents of a param *)
            let expect = "test-ä-test" in
            with_conn (fun db ->
                execute db ~params:[ Pgx.Value.of_string expect ] "SELECT $1::VARCHAR"
                >>| function
                | [ [ result ] ] ->
                  Alcotest.(check (option string))
                    ""
                    (Some expect)
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
                  Alcotest.(check (option string))
                    ""
                    (Some expect)
                    (Pgx.Value.to_string result)
                | _ -> assert false))
      ]
    in
    if force_tests || have_pg_config
    then Alcotest_io.run "pgx_test" [ "pgx_async", tests ]
    else print_endline "Skipping PostgreSQL tests since PGUSER is unset."
  ;;
end
