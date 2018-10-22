open Sexplib.Conv
open Pgx_aux

type t = string option [@@deriving sexp_of]

exception Conversion_failure of string [@@deriving sexp]

let convert_failure type_ s =
  Conversion_failure (Printf.sprintf "Unable to convert to %s: %s" type_ s)
  |> raise

let required f = function
  | Some x -> f x
  | None -> raise (Conversion_failure "Expected not-null but got null")

let opt = Option.bind

let null = None

let of_bool = function
  | true -> Some "t"
  | false -> Some "f"

let to_bool' = function
  | "t" -> true
  | "f" -> false
  | s -> convert_failure "bool" s

let to_bool_exn = required to_bool'

let to_bool = Option.map to_bool'

let of_float' f =
  match classify_float f with
  | FP_infinite when f > 0. -> "Infinity"
  | FP_infinite when f < 0. -> "-Infinity"
  | FP_nan -> "NaN"
  | _ -> string_of_float f

let of_float f =
  Some (of_float' f)

let to_float' t =
  match String.lowercase_ascii t with
  | "infinity" -> infinity
  | "-infinity" -> neg_infinity
  | "nan" -> nan
  | _ ->
    try float_of_string t
    with Failure _ -> convert_failure "float" t

let to_float_exn = required to_float'

let to_float = Option.map to_float'

type hstore = (string * string option) list [@@deriving sexp]

let of_hstore hstore =
  let string_of_quoted str = "\"" ^ str ^ "\"" in
  let string_of_mapping (key, value) =
    let key_str = string_of_quoted key
    and value_str = match value with
      | Some v -> string_of_quoted v
      | None -> "NULL"
    in key_str ^ "=>" ^ value_str
  in
  Some (String.join ", " (List.map string_of_mapping hstore))

let to_hstore' str =
  let expect target stream =
    if List.exists (fun c -> c <> Stream.next stream) target
    then convert_failure "hstore" str in
  let parse_quoted stream =
    let rec loop accum stream = match Stream.next stream with
      | '"'  -> String.implode (List.rev accum)
      (* FIXME: Slashes don't seem to round-trip properly *)
      | '\\' -> loop (Stream.next stream :: accum) stream
      | x    -> loop (x :: accum) stream in
    expect ['"'] stream;
    loop [] stream in
  let parse_value stream = match Stream.peek stream with
    | Some 'N' -> (expect ['N'; 'U'; 'L'; 'L'] stream; None)
    | _        -> Some (parse_quoted stream) in
  let parse_mapping stream =
    let key = parse_quoted stream in
    expect ['='; '>'] stream;
    let value = parse_value stream in
    (key, value) in
  let parse_main stream =
    let rec loop accum stream =
      let mapping = parse_mapping stream in
      match Stream.peek stream with
      | Some _ -> (expect [','; ' '] stream; loop (mapping :: accum) stream)
      | None   -> mapping :: accum in
    match Stream.peek stream with
    | Some _ -> loop [] stream
    | None   -> [] in
  parse_main (Stream.of_string str)

let to_hstore_exn = required to_hstore'

let to_hstore = Option.map to_hstore'

type inet = Unix.inet_addr * int

let sexp_of_inet (addr, mask) =
  [%sexp_of: string * int] (Unix.string_of_inet_addr addr, mask)

let of_inet (addr, mask) =
  let hostmask =
    if Unix.domain_of_sockaddr (Unix.ADDR_INET(addr, 1)) = Unix.PF_INET6
    then 128
    else 32
  in
  let addr = Unix.string_of_inet_addr addr
  in
  if mask = hostmask
  then Some addr
  else if mask >= 0 && mask < hostmask
  then Some (addr ^ "/" ^ string_of_int mask)
  else invalid_arg "mask"

let to_inet' =
  let re =
    let open Re in
    [ group (
        [ rep (compl [set ":./"])
        ; group (set ":.")
        ; rep1 (compl [char '/']) ]
        |> seq
      )
    ; opt (seq [char '/'; group (rep1 any)]) ]
    |> seq
    |> compile in
  fun str ->
    try
      let subs = Re.exec re str in
      let addr = Unix.inet_addr_of_string (Re.get subs 1) in
      (* optional match *)
      let mask = try (Re.get subs 3) with Not_found -> "" in
      if mask = ""
      then (addr, (if (Re.get subs 2) = "." then 32 else 128))
      else (addr, int_of_string mask)
    with _ -> convert_failure "inet" str

let to_inet_exn = required to_inet'

let to_inet = Option.map to_inet'

let of_int i =
  Some (string_of_int i)

let to_int' t =
  try int_of_string t
  with Failure _ -> convert_failure "int" t

let to_int_exn = required to_int'

let to_int = Option.map to_int'

let of_int32 i =
  Some (Int32.to_string i)

let to_int32' t =
  try Int32.of_string t
  with Failure _ -> convert_failure "int32" t

let to_int32_exn = required to_int32'

let to_int32 = Option.map to_int32'

let of_int64 i =
  Some (Int64.to_string i)

let to_int64' t =
  try Int64.of_string t
  with Failure _ -> convert_failure "int64" t

let to_int64_exn = required to_int64'

let to_int64 = Option.map to_int64'

let escape_string str =
  let buf = Buffer.create 128 in
  for i = 0 to String.length str - 1 do
    match str.[i] with
    | '"' | '\\' as x -> Buffer.add_char buf '\\'; Buffer.add_char buf x
    | x -> Buffer.add_char buf x
  done;
  Buffer.contents buf

let of_list (xs : t list) =
  let buf = Buffer.create 128 in
  Buffer.add_char buf '{';
  let adder i x =
    if i > 0 then Buffer.add_char buf ',';
    match x with
    | Some x ->
      let x = escape_string x in
      Buffer.add_char buf '"';
      Buffer.add_string buf x;
      Buffer.add_char buf '"'
    | None ->
      Buffer.add_string buf "NULL" in
  List.iteri adder xs;
  Buffer.add_char buf '}';
  Some (Buffer.contents buf)

let to_list' str =
  let n = String.length str in
  if n = 0 || str.[0] <> '{' || str.[n-1] <> '}' then
    convert_failure "list" str;
  let str = String.sub str 1 (n-2) in
  let buf = Buffer.create 128 in
  let add_field accum =
    let x = Buffer.contents buf in
    Buffer.clear buf;
    let field =
      if x = "NULL"
      then
        None
      else
        let n = String.length x in
        if n >= 2 && x.[0] = '"'
        then Some (String.sub x 1 (n-2))
        else Some x in
    field :: accum in
  let loop (accum, quoted, escaped) = function
    | '\\' when not escaped -> (accum, quoted, true)
    | '"' when not escaped ->
      Buffer.add_char buf '"'; (accum, not quoted, false)
    | ',' when not escaped && not quoted -> (add_field accum, false, false)
    | x -> Buffer.add_char buf x; (accum, quoted, false) in
  let (accum, _, _) = String.fold_left loop ([], false, false) str in
  let accum = if Buffer.length buf = 0 then accum else add_field accum in
  List.rev accum

let to_list_exn = required to_list'

let to_list = Option.map to_list'

type point = float * float [@@deriving sexp]

let of_point (x, y) =
  let x = of_float' x in
  let y = of_float' y in
  Some (Printf.sprintf "(%s,%s)" x y)

let to_point' =
  let point_re =
    let open Re in
    let part = seq [ rep space ; group (rep any) ; rep space ] in
    [ rep space ; char '(' ; part ; char ',' ; part ; char ')' ; rep space ]
    |> seq
    |> whole_string
    |> compile
  in
  fun str ->
    try
      let subs = Re.exec point_re str in
      (float_of_string (Re.get subs 1), float_of_string (Re.get subs 2))
    with
    | e -> Printexc.to_string e |> print_endline; convert_failure "point" str

let to_point_exn = required to_point'

let to_point = Option.map to_point'

let of_string t = Some t

let to_string t = t
let to_string_exn = required to_string

let unit = Some ""

let to_unit' = function
  | "" -> ()
  | t -> convert_failure "unit" t

let to_unit_exn = required to_unit'

let to_unit = Option.map to_unit'

type uuid = Uuidm.t

let sexp_of_uuid u =
  Uuidm.to_string u |> sexp_of_string

let of_uuid s =
  Some (Uuidm.to_string s)

let to_uuid' t =
  match Uuidm.of_string t with
  | Some u -> u
  | None -> convert_failure "uuid" t

let to_uuid_exn = required to_uuid'

let to_uuid = Option.map to_uuid'
