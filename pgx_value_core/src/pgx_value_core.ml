open Core_kernel
include Pgx.Value

let of_time t =
  (*
    Postgres behaves differently depending on whether the timestamp data type
    includes the timezone or not:

    Without timezone all inserted timezones are ignored
    2016-06-07 15:37:46 (no timezone)
    2016-06-07 15:37:46Z (utc timezone)
    2016-06-07 15:37:46-04 (local timezone)
    Get inserted as
    2016-06-07 15:37:46

    With timezones:
    2016-06-07 15:37:46 (no timezone) -> 2016-06-07 15:37:46-04
    2016-06-07 15:37:46Z (utc timezone) -> 2016-06-07 11:37:46-04
    2016-06-07 15:37:46-04 (local timezone) -> 2016-06-07 15:37:46-04
  *)
  Some (Time.to_string_abs ~zone:Time.Zone.utc t)
;;

let to_time' =
  (*
       The time string can come in various forms depending on whether the
       Postgres timestamp used includes the time zone:

       Without timezone
       2016-06-07 15:37:46
       2016-06-07 15:37:46.962425

       With timezone
       2016-06-07 15:37:46-04
       2016-06-07 15:37:46.962425-04

       For the first one we need to indicate that it's a UTC time by appending
       a 'Z'. For the second one we need to append the minutes to the timezone.
       Without these formattings Time.of_string fails spectacularly
    *)
  let open Re in
  let tz = seq [ alt [ char '-'; char '+' ]; digit; digit ] in
  let utctz = seq [ char 'Z'; eol ] |> compile in
  let localtz_no_min = seq [ tz; eol ] |> compile in
  let localtz = seq [ tz; char ':'; digit; digit; eol ] |> compile in
  fun s ->
    Time.of_string
    @@
    match matches utctz s, matches localtz s, matches localtz_no_min s with
    | [], [], [] -> s ^ "Z"
    | _, [], [] -> s
    | [], _, [] -> s
    | [], [], _ -> s ^ ":00"
    (* It either finishes in one of the patterns above or it doesn't *)
    | _ -> convert_failure "time" s
;;

let to_time_exn = required to_time'
let to_time = Option.map ~f:to_time'
let of_date d = Some (Date.to_string d)
let to_date' = Date.of_string
let to_date_exn = required to_date'
let to_date = Option.map ~f:to_date'
