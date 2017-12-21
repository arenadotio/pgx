(** Async based Postgres client based on Pgx. *)
open Core
open Async

include (Pgx.S with type 'a monad = 'a Deferred.t)

(* for testing purposes *)
module Thread : Pgx.IO with type 'a t = 'a Deferred.t

val with_conn: ?database:string -> (t -> 'b Deferred.t) -> 'b Deferred.t

module Value : sig
  include Pgx_value_intf.S

  val of_date : Date.t -> t
  val to_date_exn : t -> Date.t
  val to_date : t -> Date.t option

  val of_time : Time.t -> t
  val to_time_exn : t -> Time.t
  val to_time : t -> Time.t option
end
