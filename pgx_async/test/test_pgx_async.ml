open Core
open Async

include Pgx_test.Make_tests (Pgx_async.Thread)

let () =
  Scheduler.go_main ~main:(fun () ->
    ignore
      (run_tests ()
       >>| fun () ->
       Async.shutdown 0)) ()
  |> never_returns
