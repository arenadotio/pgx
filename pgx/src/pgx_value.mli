include Pgx_value_intf.S

(* Exposed for extending this module *)

(** [convert_failure type_ str] raises [Convert_failure] with a useful
    error message. *)
val convert_failure : string -> string -> _
