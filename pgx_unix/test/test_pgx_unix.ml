module Alcotest_io = struct
  type 'a test_case = 'a Alcotest.test_case

  let test_case name speed f = Alcotest.test_case name speed f
  let run name tests = Alcotest.run name tests
end

include Pgx_test.Make_tests (Pgx_unix.Simple_thread) (Alcotest_io)

let () = run_tests ()
