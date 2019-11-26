open Printf
open Sexplib0.Sexp_conv

open Pgx_aux

type t =
  { code: string
  ; severity: string
  ; message:  string
  ; custom: (char * string) list }
[@@deriving sexp]

let should_print t ~verbose =
  if verbose < 1 then false
  else if verbose = 1 then
    match t.severity with
    | "ERROR" | "FATAL" | "PANIC" -> true
    | _ -> false
  else true

let to_string ?(verbose=false) t =
  let msg = sprintf "%s: %s: %s" t.severity t.code t.message in
  let field_info =
    if verbose then
      List.map (fun (field_type, field) ->
        sprintf "%c: %s" field_type field) t.custom
    else [] in
  String.concat "\n" (msg :: field_info)
