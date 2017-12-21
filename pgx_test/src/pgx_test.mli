open OUnit2

module type S = sig
  type 'a monad
  val run_tests : unit -> unit monad
end

module Make_tests : functor (IO : Pgx.IO) -> S with type 'a monad = 'a IO.t
