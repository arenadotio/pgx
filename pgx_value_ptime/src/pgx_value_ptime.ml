include Pgx.Value


let of_date d =
  let (year, month, day) = d in
  Printf.sprintf "%04d-%02d-%02d" year month day |> Pgx.Value.of_string


let to_date' text =
  match String.split_on_char '-' text with
  | [y; m; d] -> (int_of_string y, int_of_string m, int_of_string d)
  | _ -> convert_failure "time" text


let to_date_exn v = Pgx.Value.to_string_exn v |> to_date'


let to_date v = Pgx.Value.to_string v |> Option.map to_date'


let of_time ?tz_offset_s t =
  let offset = Option.value tz_offset_s ~default:0 in
  Ptime.to_rfc3339 ~tz_offset_s:offset ~frac_s:12 t |> Pgx.Value.of_string


let time_of_string text =
  match Ptime.of_rfc3339 text with
    | Result.Ok (t, None, _) -> (t, 0)
    | Result.Ok (t, Some s, _) -> (t, s)
    | _ -> convert_failure "time" text


let to_time' text =
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
    *)
  let open Re in
  let tz = seq [ alt [ char '-'; char '+' ]; digit; digit ] in
  let utctz = seq [ char 'Z'; eol ] |> compile in
  let localtz_no_min = seq [ tz; eol ] |> compile in
  let localtz = seq [ tz; char ':'; digit; digit; eol ] |> compile in
  Printf.printf "A: %s\n" text;
  time_of_string
    @@
    match matches utctz text, matches localtz text, matches localtz_no_min text with
    | [], [], [] -> Printf.printf "%s" "B\n"; text ^ "Z"
    | _, [], [] -> Printf.printf "%s" "C\n"; text
    | [], _, [] -> Printf.printf "%s" "D\n"; text
    | [], [], _ -> Printf.printf "%s" "E\n"; text ^ ":00"
    (* It either finishes in one of the patterns above or it doesn't *)
    | _ -> convert_failure "time" text


let to_time_exn v = Pgx.Value.to_string_exn v |> to_time'
let to_time v = Pgx.Value.to_string v |> Option.map to_time'
