(** Async based Postgres client based on Pgx. *)
open Async_kernel

include
  Pgx.S
    with type 'a Io.t = 'a Deferred.t
     and type Io.ssl_config = Conduit_async.Ssl.config

(* for testing purposes *)
module Thread : Pgx.Io with type 'a t = 'a Deferred.t

(** Like [execute] but returns a pipe so you can operate on the results before they have all returned.
    Note that [execute_iter] and [execute_fold] can perform significantly better because they don't have
    as much overhead. *)
val execute_pipe : ?params:Pgx.row -> t -> string -> Pgx.row Pipe.Reader.t

(** Exposed for backwards compatiblity. New code should use [Pgx_value_core] directly. *)
module Value = Pgx_value_core
