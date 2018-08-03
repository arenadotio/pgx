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

let%test_module "to_string and should_print inline tests" =
  (module struct
    let info_msg =
      { code = "5"
      ; severity = "INFO"
      ; message = "test"
      ; custom = [('a', "string"); ('c', "field")] }
    let error_msg = { info_msg with severity = "ERROR" }

    let%test_unit "to_string tests: print msg when verbose = false" =
      let verbose = false in
      [%test_result: string] ~expect:"INFO: 5: test" (to_string ~verbose info_msg);
      [%test_result: string] ~expect:"ERROR: 5: test" (to_string ~verbose error_msg)

    let%test_unit "to_string tests: print msg and fields when verbose = true" =
      let verbose = true in
      [%test_result: string] ~expect:"ERROR: 5: test\na: string\nc: field"
        (to_string ~verbose error_msg)

    let%test_unit "should_print tests: should not print when verbose = 0" =
      let verbose = 0 in
      [%test_result: bool] ~expect:false (should_print ~verbose info_msg);
      [%test_result: bool] ~expect:false (should_print ~verbose error_msg)

    let%test_unit "should_print tests: print if verbose = 1 and t.severity \
                   is one of three: INFO, ERROR, PANIC" =
      let verbose = 1 in
      [ "FATAL" ; "ERROR" ; "PANIC" ]
      |> List.iter (fun severity ->
        let msg = { info_msg with severity } in
        [%test_result: bool] ~expect:true (should_print msg ~verbose));
      [%test_result: bool] ~expect:false (should_print info_msg ~verbose)

    let%test_unit "should_print tests: print if verbose > 1 no matter t.severity" =
      let verbose = 2 in
      [ "INFO"; "FATAL" ; "ERROR" ; "PANIC" ]
      |> List.iter (fun severity ->
        let msg = { info_msg with severity } in
        [%test_result: bool] ~expect:true (should_print msg ~verbose));
  end)
