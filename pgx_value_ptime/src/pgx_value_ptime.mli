(** Pgx_value types using Ptime's Date and Time modules

To use Ptime in utop, first run: #require "ptime";;
*)

type v = Pgx.Value.v [@@deriving compare, sexp_of]
type t = Pgx.Value.t [@@deriving compare, sexp_of]

include module type of Pgx.Value with type v := v and type t := t

val of_date : Ptime.date -> t
val to_date_exn : t -> Ptime.date
val to_date : t -> Ptime.date option

val of_time : ?tz_offset_s:Ptime.tz_offset_s -> Ptime.t -> t
val to_time_exn : t -> Ptime.t * Ptime.tz_offset_s
val to_time : t -> (Ptime.t * Ptime.tz_offset_s) option

val time_of_string : string -> Ptime.t * Ptime.tz_offset_s
