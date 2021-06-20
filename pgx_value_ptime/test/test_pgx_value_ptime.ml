module Value = Pgx_value_ptime

(* Show both an human-readable version of the date and the underlying
   seconds/offset pair for the input datetime.*)
let print_time (t, tz_offset_s) =
  let sec = Ptime.to_float_s t
  and txt = Ptime.to_rfc3339 t ~tz_offset_s ~frac_s:6 in
  Printf.sprintf "<%s | Seconds: %f, Offset: %d>" txt sec tz_offset_s

let value_testable =
  let print_time value = match Pgx.Value.to_string value with
    | Some text -> text
    | None -> "<None>" in
  let formatter ppf value = Format.pp_print_string ppf (print_time value) in
  Alcotest.testable formatter (=)


let check_value = Alcotest.check value_testable


let test_to_date _ =
  let check_date = Alcotest.(check (triple int int int)) in
  let value = Pgx.Value.of_string "2021-11-14" in
  let expected = (2021, 11, 14) in
  check_date "check date parsing" expected (Value.to_date_exn value);
  let value = Pgx.Value.of_string "0900-06-13" in
  let expected = (900, 6, 13) in
  check_date "check date with leading zeros" expected (Value.to_date_exn value)


let test_of_date _ =
  let date = (2021, 11, 14) in
  let expected = Pgx.Value.of_string "2021-11-14" in
  check_value "check date rendering" expected (Value.of_date date);
  let date = (900, 6, 13) in
  let expected = Pgx.Value.of_string "0900-06-13" in
  check_value "dates with leading zeros render properly" expected (Value.of_date date)


let date_tests =
  [
    Alcotest.test_case "of_date renders a Ptime date to a Pgx Value" `Quick test_of_date;
    Alcotest.test_case "to_date parses a Pgx Value to a Ptime date" `Quick test_to_date
  ]


(* Show only the human-readable version of the date. *)
let check_time =
  let compare_times (t1, o1) (t2, o2) =
    let tf1 = Ptime.to_float_s t1
    and tf2 = Ptime.to_float_s t2 in
    (abs_float (tf1 -. tf2) < 1.0e-6) && (o1 = o2) in
  let time_testable =
    Alcotest.testable (fun ppf t -> Format.pp_print_string ppf (print_time t)) compare_times in
  Alcotest.check time_testable


let test_time_of_string _ =
  let hour = 3600 in
  let hourf = 3600. in
  let to_pt x = Ptime.of_float_s x |> Option.value ~default:Ptime.min in
  check_time "minimum time parses" (Ptime.min, 0) (Value.time_of_string "0000-01-01T00:00:00Z");
  let pt = to_pt (12. *. hourf) in
  check_time "time with tz offset parses" (pt, ~-4 * hour) (Value.time_of_string "1970-01-01T08:00:00-04:00");
  let pt = to_pt (12. *. hourf +. 0.12345) in
  check_time "a time with milliseconds parses" (pt, 0) (Value.time_of_string "1970-01-01T12:00:00.12345Z");
  (* On linux run TZ='UTC' date -d @1458086118 to confirm this conversion is correct.*)
  check_time "a recent time parses" (to_pt 1458086118., ~-4 * hour) (Value.time_of_string "2016-03-15 19:55:18-04:00")


let time_roundtrip str = Value.of_string str |> Value.to_time_exn

let test_time_tz_handling _ =
  let (utc_t, tz_offset_s) = Value.time_of_string "2016-03-15 19:55:18-04:00" in
  check_time "without TZ" (utc_t, 0) (time_roundtrip "2016-03-15 23:55:18");
  check_time "zulu" (utc_t, 0) (time_roundtrip "2016-03-15 23:55:18Z");
  check_time "hour TZ" (utc_t, tz_offset_s) (time_roundtrip "2016-03-15 19:55:18-04");
  check_time "full TZ" (utc_t, tz_offset_s) (time_roundtrip "2016-03-15 19:55:18-04:00")


let test_time_conversion_roundtrip _ =
  let print_time (t, tz_offset_s) = Ptime.to_rfc3339 t ~tz_offset_s ~frac_s:6 in
  let expected_str = "2016-03-15T23:55:18.123456Z" in
  Alcotest.(check string) "parse-print" expected_str (time_roundtrip expected_str |> print_time);
  let (t, tz_offset_s) = Value.time_of_string expected_str in
  let actual = Value.of_time t ~tz_offset_s |> Value.to_time_exn in
  check_time "print-parse" (t, tz_offset_s) actual


let time_tests =
  [ Alcotest.test_case "test time_of_string" `Quick test_time_of_string
  ; Alcotest.test_case
      "test time_of_string time zone handling"
      `Quick
      test_time_tz_handling
  ; Alcotest.test_case
      "test time conversion roundtrip"
      `Quick
      test_time_conversion_roundtrip
  ]


let () = Alcotest.run "pgx_async_conversions" [
  "date", date_tests;
  "time", time_tests
]
