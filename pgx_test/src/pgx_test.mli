module type S = sig
  type 'a monad

  val run_tests : unit -> unit
end

module type ALCOTEST_IO = sig
  open Alcotest

  type 'a monad
  type 'a test_case

  val test_case : string -> speed_level -> ('a -> unit monad) -> 'a test_case
  val run : string -> (string * unit test_case list) list -> unit
end

module Make_tests (IO : Pgx.IO) (Alcotest_io : ALCOTEST_IO with type 'a monad := 'a IO.t) :
  S with type 'a monad = 'a IO.t
