(** Pgx_value types using Core_kernel's Date and Time modules *)
open Core_kernel

include Pgx_value_intf.S

val of_date : Date.t -> t
val to_date_exn : t -> Date.t
val to_date : t -> Date.t option
val of_time : Time.t -> t
val to_time_exn : t -> Time.t
val to_time : t -> Time.t option
