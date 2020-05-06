open Base

type oid = int32 [@@deriving compare, sexp]

(** None is NULL. *)
type param = Pgx_value.t [@@deriving compare, sexp_of]

(** None is NULL. *)
type result = Pgx_value.t [@@deriving compare, sexp_of]

(** One row is a list of fields. *)
type row = Pgx_value.t list [@@deriving compare, sexp_of]

type params_description = oid list [@@deriving compare, sexp]

exception PostgreSQL_Error of string * Error_response.t [@@deriving sexp]
