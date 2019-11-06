open Types

type t =
  { name : string   (** Field name. *)
  ; table : oid option   (** OID of table. *)
  ; column : int option   (** Column number of field in table. *)
  ; field_type : oid   (** The type of the field. *)
  ; length : int    (** Length of the field. *)
  ; modifier : int32   (** Type modifier. *)
  }
[@@deriving sexp]

val of_row_desc : Row_desc.t -> t
