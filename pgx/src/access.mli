type t = Read_write | Read_only [@@deriving sexp]

val to_string : t -> string
