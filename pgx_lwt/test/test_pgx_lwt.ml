module Alcotest_io = struct
  let test_case name speed f = Alcotest_lwt.test_case name speed (fun _ -> f)
end
include Pgx_test.Make_tests (Pgx_lwt.Thread) (Alcotest_io)

let () = run_tests ()
