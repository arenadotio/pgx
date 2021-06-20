include Pgx.Value


let of_date (year, month, day) =
  Printf.sprintf "%04d-%02d-%02d" year month day |> Pgx.Value.of_string

let to_date' text =
  match text ^ "T00:00:00Z" |> Ptime.of_rfc3339 with
  | Result.Ok (t, _, _) -> Ptime.to_date t
  | _ -> convert_failure "date" text

let to_date_exn v = Pgx.Value.to_string_exn v |> to_date'
let to_date v = Pgx.Value.to_string v |> Option.map to_date'


let of_time ?tz_offset_s t =
  let tz_offset_s = Option.value tz_offset_s ~default:0 in
  Ptime.to_rfc3339 ~tz_offset_s ~frac_s:12 t |> Pgx.Value.of_string


let time_of_string text =
  match Ptime.of_rfc3339 text with
  | Result.Ok (t, offset, _) -> (t, Option.value ~default:0 offset)
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
  let localtz = seq [ tz; char ':'; digit; digit; eol ] |> compile in
  let localtz_no_min = seq [ tz; eol ] |> compile in
  time_of_string
    @@
    match matches utctz text, matches localtz text, matches localtz_no_min text with
    | [], [], [] -> text ^ "Z"
    | _, _, [] -> text
    | [], [], _ -> text ^ ":00"
    | _ -> convert_failure "time" text


let to_time_exn v = Pgx.Value.to_string_exn v |> to_time'
let to_time v = Pgx.Value.to_string v |> Option.map to_time'
