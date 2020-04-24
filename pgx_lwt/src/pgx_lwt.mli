(** Lwt based Postgres client based on Pgx. *)

include Pgx.S with type 'a monad = 'a Lwt.t

(* for testing purposes *)
module Thread : Pgx.IO with type 'a t = 'a Lwt.t
