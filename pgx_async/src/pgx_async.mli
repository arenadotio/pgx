(** Async based Postgres client based on Pgx. *)
open Core
open Async

include (Pgx.S with type 'a monad = 'a Deferred.t)

(* for testing purposes *)
module Thread : Pgx.IO with type 'a t = 'a Deferred.t

val with_conn
  : ?host:string
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

module Value : sig
  include Pgx_value_intf.S

  val of_date : Date.t -> t
  val to_date_exn : t -> Date.t
  val to_date : t -> Date.t option

  val of_time : Time.t -> t
  val to_time_exn : t -> Time.t
  val to_time : t -> Time.t option
end
