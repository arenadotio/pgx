(** Async based Postgres client based on Pgx. *)
open Async

include Pgx.S with type 'a monad = 'a Deferred.t

(* for testing purposes *)
module Thread : Pgx.IO with type 'a t = 'a Deferred.t

val with_conn
  :  ?host:string
  -> ?port:int
  -> ?user:string
  -> ?password:string
  -> ?database:string
  -> ?unix_domain_socket_dir:string
  -> ?verbose:int
  -> ?max_message_length:int
  -> (t -> 'a Deferred.t)
  -> 'a Deferred.t

(** Like [execute] but returns a pipe so you can operate on the results before they have all returned.
    Note that [execute_iter] and [execute_fold] can perform significantly better because they don't have
    as much overhead. *)
val execute_pipe : ?params:Pgx.row -> t -> string -> Pgx.row Pipe.Reader.t

(** Exposed for backwards compatiblity. New code should use [Pgx_value_core] directly. *)
module Value = Pgx_value_core
