open Pgx_aux
open Printf
open Sexplib0
open Sexplib0.Sexp_conv
open Pgx.Value

let pp_value ppf x = Sexp.pp_hum ppf (sexp_of_t x)
let equal_value (x : t) (y : t) = x = y
let pp_hstore ppf x = Sexp.pp_hum ppf (sexp_of_hstore x)
let equal_hstore x y = Sexp.equal (sexp_of_hstore x) (sexp_of_hstore y)
let printer sexp value = sexp value |> Sexp.to_string_hum
let sort_hstore = List.sort (fun (k, _) (k', _) -> String.compare k k')
let to_hstore_sorted v = to_hstore v |> Option.map sort_hstore
let to_hstore_sorted_exn v = to_hstore_exn v |> sort_hstore
let pp_inet ppf (addr, port) = Format.fprintf ppf "%a:%d" Ipaddr.pp addr port
let equal_inet (a1, p1) (a2, p2) = Ipaddr.compare a1 a2 = 0 && p1 = p2
let epsilon = 0.00001

let equal_float x y =
  match classify_float x, classify_float y with
  | FP_infinite, FP_infinite -> x = y
  | FP_nan, FP_nan -> true
  | _, _ -> abs_float (x -. y) <= epsilon *. (abs_float x +. abs_float y)
;;

module Alcotest_ext = struct
  let hstore = Alcotest.testable pp_hstore equal_hstore
  let inet = Alcotest.testable pp_inet equal_inet
  let value = Alcotest.testable pp_value equal_value
  let uuid = Alcotest.testable Uuidm.pp Uuidm.equal
  let our_float = Alcotest.testable Format.pp_print_float equal_float
end

let make_test name typ to_value of_value of_value_exn values fail_values =
  let fail_tests =
    Alcotest.test_case "null required input" `Quick (fun () ->
        Alcotest.check_raises
          "non-null conversion"
          (Conversion_failure "Expected not-null but got null")
          (fun () -> ignore (of_value_exn None)))
    :: List.map
         (fun str ->
           let test_name = sprintf "bad conversion - %s" str in
           let value = of_string str in
           Alcotest.test_case test_name `Quick
           @@ fun () ->
           let msg = sprintf "Unable to convert to %s: %s" name str in
           Alcotest.check_raises "conversion error" (Conversion_failure msg) (fun () ->
               ignore (of_value value)))
         fail_values
  in
  let success_opt_tests =
    None :: List.map (fun v -> Some v) values
    |> List.map (fun expect ->
           let test_name =
             Format.asprintf "good conversion - %a" Alcotest.(pp (option typ)) expect
           in
           Alcotest.test_case test_name `Quick
           @@ fun () ->
           let value = expect |> opt to_value |> of_value in
           Alcotest.(check (option typ)) test_name expect value)
  in
  let success_tests =
    List.map
      (fun expect ->
        let test_name = Format.asprintf "good conversion - %a" (Alcotest.pp typ) expect in
        Alcotest.test_case test_name `Quick
        @@ fun () ->
        let value = expect |> to_value |> of_value_exn in
        Alcotest.(check typ) test_name expect value)
      values
  in
  name, success_tests @ success_opt_tests @ fail_tests
;;

let () =
  let all_chars = String.init 255 char_of_int in
  Alcotest.run
    "Pgx.Value"
    [ make_test
        "bool"
        Alcotest.bool
        of_bool
        to_bool
        to_bool_exn
        [ true; false ]
        [ ""; "asd" ]
    ; make_test
        "float"
        Alcotest_ext.our_float
        of_float
        to_float
        to_float_exn
        [ 0.; 3.14; -5.; neg_infinity; infinity; nan; max_float; min_float ]
        [ ""; "asd" ]
    ; make_test
        "hstore"
        Alcotest_ext.hstore
        of_hstore
        to_hstore_sorted
        to_hstore_sorted_exn
        [ []
        ; [ "a", Some "b" ]
        ; [ "key", None ]
        ; [ "1", Some "2"; "3;'", Some "'!"; "asdf=>", None ]
        ]
        [ "asd"; "=>"; "a=>"; "=>v" ]
    ; make_test
        "inet"
        Alcotest_ext.inet
        of_inet
        to_inet
        to_inet_exn
        ([ "127.0.0.1", 32; "192.168.5.9", 0; "fe80::0202:b3ff:fe1e:8329", 128 ]
        |> List.map (fun (addr, mask) -> Ipaddr.of_string_exn addr, mask))
        [ ""; "asd"; "192.168.1.a/32" ]
    ; make_test
        "int"
        Alcotest.int
        of_int
        to_int
        to_int_exn
        [ 0; 1; -1; max_int; min_int ]
        [ ""; "asd"; "t"; "f" ]
    ; make_test
        "int32"
        Alcotest.int32
        of_int32
        to_int32
        to_int32_exn
        Int32.[ zero; of_int 1; of_int (-1); max_int; min_int ]
        [ ""; "asd"; "t"; "f" ]
    ; make_test
        "int64"
        Alcotest.int64
        of_int64
        to_int64
        to_int64_exn
        Int64.[ zero; of_int 1; of_int (-1); max_int; min_int ]
        [ ""; "asd"; "t"; "f" ]
    ; make_test
        "list"
        Alcotest.(list Alcotest_ext.value)
        of_list
        to_list
        to_list_exn
        [ []
        ; [ of_bool true
          ; of_bool false
          ; of_float 10.5
          ; of_hstore []
          ; of_hstore [ "key", Some "value" ]
          ; of_hstore [ "key2", None ]
          ; of_inet (Ipaddr.of_string_exn "8.8.8.8", 4)
          ; of_int 99
          ; of_int32 (Int32.of_int 101)
          ; of_int64 (Int64.of_int 1102931)
          ; of_list []
          ; null
          ; of_point (-5., 100.)
          ; unit
          ; of_uuid (Uuidm.create `V4)
          ; of_string all_chars
          ]
        ]
        [ ""; "asd" ]
    ; make_test
        "point"
        Alcotest.(Alcotest_ext.(pair our_float our_float))
        of_point
        to_point
        to_point_exn
        [ 0., 0.; infinity, neg_infinity; nan, nan; max_float, 5.; -5., max_float ]
        [ ""; "asd"; "5." ]
    ; make_test
        "string"
        Alcotest.string
        of_string
        to_string
        to_string_exn
        [ ""; "this is a test string"; all_chars ]
        []
    ; make_test "unit" Alcotest.unit (fun () -> unit) to_unit to_unit_exn [ () ] [ "asd" ]
    ; make_test
        "uuid"
        Alcotest_ext.uuid
        of_uuid
        to_uuid
        to_uuid_exn
        [ Uuidm.create `V4 ]
        [ ""; "asd" ]
    ]
;;
