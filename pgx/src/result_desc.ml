open Sexplib.Conv

include Types

type t =
  { name : string
  ; table : oid option
  ; column : int option
  ; field_type : oid
  ; length : int
  ; modifier : int32 }
[@@deriving sexp]

let of_row_desc r =
  let open Row_desc in
  { name = r.name
  ; table = if r.table = 0l then None else Some r.table
  ; column = if r.col = 0 then None else Some r.col
  ; field_type = r.oid
  ; length = r.len
  ; modifier = r.modifier }
