include Pgx_test.Make_tests (Pgx_lwt.Thread)

let () = run_tests () |> Lwt_main.run
