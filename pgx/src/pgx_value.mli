include Pgx_value_intf.S

(* Exposed for extending this module *)

(** [convert_failure type_ str] raises [Convert_failure] with a useful
    error message. Add [~hint] if there's additional info you can give the
    user about the error. *)
val convert_failure : ?hint:string -> string -> string -> _
