open Sexplib0.Sexp_conv

type oid = int32 [@@deriving sexp]

(** None is NULL. *)
type param = Pgx_value.t [@@deriving sexp_of]

(** None is NULL. *)
type result = Pgx_value.t [@@deriving sexp_of]

(** One row is a list of fields. *)
type row = Pgx_value.t list [@@deriving sexp_of]

type params_description = oid list [@@deriving sexp]

exception PostgreSQL_Error of string * Error_response.t [@@deriving sexp]
