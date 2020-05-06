type t =
  { code : string
  ; severity : string
  ; message : string
  ; custom : (char * string) list
  }
[@@deriving compare, sexp]

val should_print : t -> verbose:int -> bool
val to_string : ?verbose:bool -> t -> string
