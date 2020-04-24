type t =
  | Serializable
  | Repeatable_read
  | Read_committed
  | Read_uncommitted
[@@deriving sexp]

let to_string = function
  | Serializable -> "serializable"
  | Repeatable_read -> "repeatable read"
  | Read_committed -> "read committed"
  | Read_uncommitted -> "read uncommitted"
;;
