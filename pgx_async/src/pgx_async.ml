open Core
open Async

(* Pgx allows to generate bindings from any module implementing their
   THREAD signature which encompasses monadic concurrency + IO. The
   implementation that we've chosen here is a deferred represents an
   asynchronous value returned by pgx and Writer.t/Reader.t are the
   channels it uses for communication *)

exception Pgx_eof [@@deriving sexp]

module Thread = struct
  type 'a t = 'a Deferred.t

  let return = return
  let (>>=) = (>>=)

  let catch f on_exn =
    try_with ~extract_exn:true f >>= function
    | Ok x -> return x
    | Error exn -> on_exn exn

  type sockaddr =
    | Unix of string
    | Inet of string * int

  type in_channel = Reader.t
  type out_channel = Writer.t

  let sexp_of_in_channel _ = Sexplib0.Sexp.Atom "<opaque>"
  let sexp_of_out_channel _ = Sexplib0.Sexp.Atom "<opaque>"

  let output_char w char = return (Writer.write_char w char)
  let output_string w s = return (Writer.write w s)
  let output_binary_int w n =
    let chr = Caml.Char.chr in
    Writer.write_char w (chr (n lsr 24));
    Writer.write_char w (chr ((n lsr 16) land 255));
    Writer.write_char w (chr ((n lsr 8) land 255));
    return @@ Writer.write_char w (chr (n land 255))
  let flush = Writer.flushed

  let input_char r =
    Reader.read_char r >>| function
    | `Ok c -> c
    | `Eof -> raise Pgx_eof
  let input_binary_int r =
    let b = Bytes.create 4 in
    Reader.really_read r b >>| function
    | `Eof _ -> raise Pgx_eof
    | `Ok ->
      let code = Caml.Char.code in
      (code (Bytes.get b 0) lsl 24) lor (code (Bytes.get b 1) lsl 16)
      lor (code (Bytes.get b 2) lsl 8) lor (code (Bytes.get b 3))
  let really_input r s pos len =
    Reader.really_read r ~pos ~len s >>| function
    | `Ok -> ()
    | `Eof _ -> raise Pgx_eof

  let close_in = Reader.close

  let open_connection sockaddr =
    let get_reader_writer socket =
      let fd = Socket.fd socket in
      (Reader.create fd, Writer.create fd) in
    match sockaddr with
    | Unix path ->
      let unix_sockaddr = Tcp.Where_to_connect.of_unix_address (`Unix path) in
      Tcp.connect_sock unix_sockaddr
      >>| get_reader_writer
    | Inet (host, port) ->
      let inet_sockaddr = Tcp.Where_to_connect.of_host_and_port
                            (Host_and_port.create ~host:host ~port:port) in
      Tcp.connect_sock inet_sockaddr
      >>| get_reader_writer

  (* The unix getlogin syscall can fail *)
  let getlogin () =
    Unix.getuid ()
    |> Unix.Passwd.getbyuid_exn
    >>| fun { name ; _ } -> name

  let debug msg =
    Log.Global.debug ~tags:["lib", "pgx_async"] "%s" msg;
    Log.Global.flushed ()

  let protect f ~finally = Monitor.protect f ~finally

  module Sequencer = struct
    type 'a monad = 'a t
    type 'a t = 'a Sequencer.t

    let sexp_of_t _ = Sexplib0.Sexp.Atom "<opaque>"

    let create t = Sequencer.create ~continue_on_error:true t

    let enqueue = Throttle.enqueue
  end
end

include (Pgx.Make(Thread))

(* pgx uses configures this value at build time. But this breaks when
   pgx is installed before postgres itself. We prefer to set this variable
   at runtime and override the `connect` function from to respect it *)
let default_unix_domain_socket_dir =
  let debian_default = "/var/run/postgresql" in
  Lazy_deferred.create (fun () ->
    Sys.is_directory debian_default >>| function
    | `Yes -> debian_default
    | `No | `Unknown -> "/tmp"
  )

(* Fail if PGDATABASE environment variable is not set. *)
let check_pgdatabase = lazy (
  let db = "PGDATABASE" in
  if Option.is_none (Sys.getenv db)
  then failwithf "%s environment variable must be set." db ())

let connect ?host ?port ?user ?password ?database ?unix_domain_socket_dir
      ?verbose ?max_message_length () =
  if Option.is_none database then Lazy.force check_pgdatabase;
  begin match unix_domain_socket_dir with
  | Some p -> return p
  | None -> Lazy_deferred.force_exn default_unix_domain_socket_dir
  end >>= fun unix_domain_socket_dir ->
  connect ?host ?port ?user ?password ?database ?verbose ?max_message_length ~unix_domain_socket_dir ()

let with_conn ?host ?port ?user ?password ?database ?unix_domain_socket_dir
      ?verbose ?max_message_length f =
  connect ?host ?port ?user ?password ?database ?unix_domain_socket_dir
    ?verbose ?max_message_length ()
  >>= fun dbh ->
  Monitor.protect (fun () -> f dbh)
    ~finally:(fun () -> close dbh)

let execute_pipe ?params db query =
  Pipe.create_reader ~close_on_exception:false @@ fun writer ->
  execute_iter ?params db query ~f:(fun row ->
    Pipe.write_if_open writer row)

module Value = struct
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
    let tz = seq [ alt [ char '-' ; char '+' ] ; digit ; digit ] in
    let utctz = seq [ char 'Z' ; eol ] |> compile in
    let localtz_no_min = seq [ tz ; eol ] |> compile in
    let localtz = seq [ tz ; char ':' ; digit ; digit ; eol ] |> compile in
    fun s ->
      Time.of_string @@
      match matches utctz s, matches localtz s, matches localtz_no_min s with
      | [], [], [] -> s ^ "Z"
      | _ , [], [] -> s
      | [], _ , [] -> s
      | [], [], _  -> s ^ ":00"
      (* It either finishes in one of the patterns above or it doesn't *)
      | _ -> convert_failure "time" s

  let to_time_exn = required to_time'

  let to_time = Option.map ~f:to_time'

  let of_date d =
    Some (Date.to_string d)

  let to_date' = Date.of_string

  let to_date_exn = required to_date'

  let to_date = Option.map ~f:to_date'
end
