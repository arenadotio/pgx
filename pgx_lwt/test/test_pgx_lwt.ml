module Alcotest_io = struct
  type 'a test_case = 'a Alcotest_lwt.test_case
  let test_case name speed f = Alcotest_lwt.test_case name speed (fun _ -> f)
  let run name tests =
    Alcotest_lwt.run name tests
    |> Lwt_main.run
end
include Pgx_test.Make_tests (Pgx_lwt.Thread) (Alcotest_io)

let () = run_tests ()
