open Sexplib0.Sexp_conv

type t =
  { name : string
  ; table : int32
  ; col : int
  ; oid : int32
  ; len : int
  ; modifier : int32
  ; format : int
  }
[@@deriving sexp]
