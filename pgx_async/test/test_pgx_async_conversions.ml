open Core
open Async
open OUnit2
module Value = Pgx_async.Value

let time_roundtrip str = Value.to_time_exn (Some str)

let printer = Time.to_string_abs ~zone:Time.Zone.utc

let test_time_of_string _ =
  let expected = Time.of_string "2016-03-15 19:55:18.123456-04:00" in
  assert_equal ~printer expected (time_roundtrip "2016-03-15 23:55:18.123456") ;
  assert_equal ~printer expected (time_roundtrip "2016-03-15 23:55:18.123456Z") ;
  assert_equal ~printer expected
    (time_roundtrip "2016-03-15 19:55:18.123456-04") ;
  assert_equal ~printer expected
    (time_roundtrip "2016-03-15 19:55:18.123456-04:00")

let test_time_of_string_no_ms _ =
  let expected = Time.of_string "2016-03-15 19:55:18-04:00" in
  assert_equal ~printer expected (time_roundtrip "2016-03-15 23:55:18") ;
  assert_equal ~printer expected (time_roundtrip "2016-03-15 23:55:18Z") ;
  assert_equal ~printer expected (time_roundtrip "2016-03-15 19:55:18-04") ;
  assert_equal ~printer expected (time_roundtrip "2016-03-15 19:55:18-04:00")

let test_time_conversion_roundtrip _ =
  let expected_str = "2016-03-15 23:55:18.123456Z" in
  assert_equal expected_str ~printer:Fn.id
    (time_roundtrip expected_str |> printer) ;
  let expected_time = Time.of_string expected_str in
  assert_equal expected_time ~printer
    (Value.of_time expected_time |> Value.to_time_exn)

let unit_tests =
  [ "test time_of_string" >:: test_time_of_string
  ; "test time_of_string no milliseconds" >:: test_time_of_string_no_ms
  ; "test time conversion roundtrip" >:: test_time_conversion_roundtrip ]

let () =
  run_test_tt_main ~exit:Caml.exit ("pgx_async_conversions" >::: unit_tests)
