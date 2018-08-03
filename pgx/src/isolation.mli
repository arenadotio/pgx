(** Database transaction isolation levels *)
type t =
  | Serializable
  | Repeatable_read
  | Read_committed
  | Read_uncommitted
[@@deriving sexp]

val to_string : t -> string
