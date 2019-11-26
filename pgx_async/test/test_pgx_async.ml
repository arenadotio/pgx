module Alcotest_io = struct
  let test_case name speed f = Alcotest_async.test_case name speed f
end

include Pgx_test.Make_tests (Pgx_async.Thread) (Alcotest_io)

let () = run_tests ()
