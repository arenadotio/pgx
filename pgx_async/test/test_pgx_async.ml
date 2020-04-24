module Alcotest_io = struct
  type 'a test_case = 'a Alcotest_async.test_case
  let test_case name speed f = Alcotest_async.test_case name speed f
  let run name tests =
    Async_unix.Thread_safe.block_on_async_exn @@ fun () ->
    Alcotest_async.run name tests
end

include Pgx_test.Make_tests (Pgx_async.Thread) (Alcotest_io)

let () = run_tests ()
