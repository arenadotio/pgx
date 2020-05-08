(** Pgx_value types using Core_kernel's Date and Time modules *)
open Core_kernel

type v = Pgx_value.v [@@deriving compare, sexp_of]
type t = Pgx_value.t [@@deriving compare, sexp_of]

include Pgx_value_intf.S with type v := v and type t := t

val of_date : Date.t -> t
val to_date_exn : t -> Date.t
val to_date : t -> Date.t option
val of_time : Time.t -> t
val to_time_exn : t -> Time.t
val to_time : t -> Time.t option
