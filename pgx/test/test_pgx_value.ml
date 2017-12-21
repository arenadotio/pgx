open OUnit2
open Pgx_aux
open Printf
open Sexplib
open Sexplib.Conv

open Pgx.Value

let printer sexp value =
  sexp value
  |> Sexp.to_string_hum

let sort_hstore =
  List.sort (fun (k, _) (k', _) ->
    compare k k')

let to_hstore_sorted v =
  to_hstore v
  |> Option.map sort_hstore

let to_hstore_sorted_exn v =
  to_hstore_exn v
  |> sort_hstore

let cmp_float a b =
  match classify_float a, classify_float b with
  | FP_nan, FP_nan
  (* This weird when check is that the two infinities have the same sign *)
  | FP_infinite, FP_infinite when (a > 0.) = (b > 0.) -> true
  | _ -> cmp_float a b

let cmp_point (x, y) (x', y') =
  if cmp_float x x' then
    cmp_float y y'
  else false

let make_test ?cmp name sexp to_value of_value of_value_exn values
      fail_values =
  let fail_tests =
    (sprintf "%s null required input" name >:: (fun _ ->
       assert_raises (Conversion_failure "Expected not-null but got null")
         (fun () ->
            of_value_exn None)))
    ::
    List.map (fun str ->
      let test_name = sprintf !"%s bad conversion - %s" name str in
      let value = of_string str in
      test_name >:: fun _ ->
        let msg = sprintf "Unable to convert to %s: %s" name str in
        assert_raises (Conversion_failure msg) (fun () -> of_value value))
      fail_values
  in
  let success_opt_tests =
    let sexp_opt = sexp_of_option sexp in
    let cmp = match cmp with
      | None -> None
      | Some cmp -> Some (Option.equal ~cmp)
    in
    None :: List.map (fun v -> Some v) values
    |> List.map (fun expect ->
      let test_name = sprintf "%s good conversion - %s"
                        name (sexp_opt expect |> Sexp.to_string_hum) in
      test_name >:: fun _ ->
        let printer = printer sexp_opt in
        let value = expect |> (opt to_value) |> of_value in
        assert_equal ?cmp ~printer expect value)
  in
  let success_tests =
    List.map (fun expect ->
      let test_name = sprintf "%s good conversion - %s"
                        name (sexp expect |> Sexp.to_string_hum) in
      test_name >:: fun _ ->
        let printer = printer sexp in
        let value = expect |> to_value |> of_value_exn in
        assert_equal ?cmp ~printer expect value)
      values
  in
  success_tests @ success_opt_tests @ fail_tests

let () =
  let all_chars = String.init 255 char_of_int in
  [ make_test "bool" sexp_of_bool
      of_bool to_bool to_bool_exn
      [ true
      ; false ]
      [ "" ; "asd" ]
  ; make_test "float" sexp_of_float
      of_float to_float to_float_exn
      ~cmp:cmp_float
      [ 0.
      ; 3.14
      ; -5.
      ; neg_infinity
      ; infinity
      ; nan
      ; max_float
      ; min_float ]
      [ "" ; "asd" ]
  ; make_test "hstore" sexp_of_hstore
      of_hstore to_hstore_sorted to_hstore_sorted_exn
      [ []
      ; [ "a", Some "b" ]
      ; [ "key", None ]
      ; [ "1", Some "2" ; "3;'", Some "'!" ; "asdf=>", None ] ]
      [ "asd" ; "=>" ; "a=>" ; "=>v" ]
  ; make_test "inet" sexp_of_inet
      of_inet to_inet to_inet_exn
      ([ "127.0.0.1", 32
       ; "192.168.5.9", 0
       ; "fe80::0202:b3ff:fe1e:8329", 128 ]
       |> List.map (fun (addr, mask) ->
         Unix.inet_addr_of_string addr, mask))
      [ "" ; "asd" ; "192.168.1.a/32" ]
  ; make_test "int" sexp_of_int
      of_int to_int to_int_exn
      [ 0
      ; 1
      ; -1
      ; max_int
      ; min_int ]
      [ "" ; "asd" ; "t" ; "f" ]
  ; make_test "int32" sexp_of_int32
      of_int32 to_int32 to_int32_exn
      Int32.([ zero
             ; of_int 1
             ; of_int (-1)
             ; max_int
             ; min_int ])
      [ "" ; "asd" ; "t" ; "f" ]
  ; make_test "int64" sexp_of_int64
      of_int64 to_int64 to_int64_exn
      Int64.([ zero
             ; of_int 1
             ; of_int (-1)
             ; max_int
             ; min_int ])
      [ "" ; "asd" ; "t" ; "f" ]
  ; make_test "list" [%sexp_of: t list]
      of_list to_list to_list_exn
      [ []
      ; [ of_bool true
        ; of_bool false
        ; of_float 10.5
        ; of_hstore []
        ; of_hstore [ "key", Some "value" ]
        ; of_hstore [ "key2", None ]
        ; of_inet (Unix.inet_addr_of_string "8.8.8.8", 4)
        ; of_int 99
        ; of_int32 (Int32.of_int 101)
        ; of_int64 (Int64.of_int 1102931)
        ; of_list []
        ; null
        ; of_point (-5., 100.)
        ; unit
        ; of_uuid (Uuidm.create `V4)
        ; of_string all_chars ] ]
      [ "" ; "asd" ]
  ; make_test "point" sexp_of_point
      of_point to_point to_point_exn
      ~cmp:cmp_point
      [ 0., 0.
      ; infinity, neg_infinity
      ; nan, nan
      ; max_float, 5.
      ; -5., max_float ]
      [ "" ; "asd" ; "5." ]
  ; make_test "string" sexp_of_string
      of_string to_string to_string_exn
      [ ""
      ; "this is a test string"
      ; all_chars ]
      []
  ; make_test "unit" sexp_of_unit
      (fun () -> unit) to_unit to_unit_exn
      [ () ]
      [ "asd" ]
  ; make_test "uuid" sexp_of_uuid
      of_uuid to_uuid to_uuid_exn
      [ Uuidm.create `V4 ]
      [ "" ; "asd" ]
  ]
  |> List.concat
  |> (>:::) "Pgx.Value"
  |> run_test_tt_main ~exit
