module type S = sig
  val run_tests : library_name:string -> unit
end

module type ALCOTEST_IO = sig
  open Alcotest

  type 'a monad
  type 'a test_case

  val test_case : string -> speed_level -> ('a -> unit monad) -> 'a test_case
  val run : string -> (string * unit test_case list) list -> unit
end

module Make_tests
    (Pgx_impl : Pgx.S)
    (Alcotest_io : ALCOTEST_IO with type 'a monad := 'a Pgx_impl.Io.t) : S
