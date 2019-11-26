module type S = sig
  type 'a monad
  val run_tests : unit -> unit
end

module type ALCOTEST_IO = sig
  open Alcotest
  type 'a monad
  val test_case : string -> speed_level -> ('a -> unit monad) -> 'a test_case
end

module Make_tests :
  functor (IO : Pgx.IO) ->
  functor (Alcotest_io : ALCOTEST_IO with type 'a monad := 'a IO.t) ->
  S with type 'a monad = 'a IO.t
