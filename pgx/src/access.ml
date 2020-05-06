type t =
  | Read_write
  | Read_only
[@@deriving compare, sexp]

let to_string = function
  | Read_write -> "read write"
  | Read_only -> "read only"
;;
