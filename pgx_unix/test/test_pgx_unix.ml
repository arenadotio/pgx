include Pgx_test.Make_tests (Pgx_unix.Simple_thread) (Alcotest)

let () =
  run_tests ()
