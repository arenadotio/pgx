type t =
  | Read_write
  | Read_only
[@@deriving compare, sexp]

val to_string : t -> string
