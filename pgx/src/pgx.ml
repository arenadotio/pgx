(* PG'OCaml is a set of OCaml bindings for the PostgreSQL database.
 *
 * PG'OCaml - type safe interface to PostgreSQL.
 * Copyright (C) 2005-2009 Richard Jones and other authors.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
*)

open Pgx_aux
open Printf
open Sexplib0.Sexp_conv

include Types

module Isolation = Isolation

module Access = Access

module Ready = struct
  type t =
    | Idle
    | In_transaction
    | Error
    | Other of char
  [@@deriving sexp]

  let of_char = function
    | 'I' -> Idle
    | 'T' -> In_transaction
    | 'E' -> Error
    | c   -> Other c
end

module Result_desc = Result_desc

(* We get a message from postgres that we didn't expect. Almost always a bug
   in our bindings *)
exception Unexpected_message of string [@@deriving sexp]
let fail_msg fmt =
  ksprintf (fun m ->
    raise (Unexpected_message m))
    fmt

exception Parsing_failure of string [@@deriving sexp]
let fail_parse str = raise (Parsing_failure str)

module Error_response = Error_response

module Message_in = struct
  type copy_format =
    | Text
    | Binary
  [@@deriving sexp]

  type t =
    | AuthenticationOk
    | AuthenticationKerberosV5
    | AuthenticationCleartextPassword
    | AuthenticationCryptPassword of string
    | AuthenticationMD5Password of string
    | AuthenticationSCMCredential
    | BackendKeyData of int32 * int32
    | BindComplete
    | CloseComplete
    | CommandComplete of string
    | CopyOutResponse of (copy_format * copy_format list)
    | CopyData of string
    | CopyDone
    | DataRow of string option list
    | EmptyQueryResponse
    | ErrorResponse of Error_response.t
    | NoData
    | NoticeResponse of (char * string) list
    | ParameterDescription of int32 list
    | ParameterStatus of string * string
    | ParseComplete
    | ReadyForQuery of Ready.t
    | RowDescription of Row_desc.t list
    | UnknownMessage of char * string
  [@@deriving sexp]

  let to_string t = Sexplib0.Sexp.to_string_hum (sexp_of_t t)

  let read (typ, msg) =
    let pos = ref 0 in
    let len = String.length msg in
    let get_char where =
      if !pos < len then (
        let r = msg.[!pos] in
        incr pos;
        r)
      else
        fail_parse ("Pgx: parse_backend_message: " ^ where ^
                    ": short message") in
    let get_byte where = Char.code (get_char where) in
    let get_int8 () =
      get_byte "get_int8" in
    let get_int16 () =
      let r0 = get_byte "get_int16" in
      let r1 = get_byte "get_int16" in
      (r0 lsr 8) + r1 in
    let get_int32 () =
      let r0 = get_byte "get_int32" in
      let r1 = get_byte "get_int32" in
      let r2 = get_byte "get_int32" in
      let r3 = get_byte "get_int32" in
      let r = Int32.of_int r0 in
      let r = Int32.shift_left r 8 in
      let r = Int32.logor r (Int32.of_int r1) in
      let r = Int32.shift_left r 8 in
      let r = Int32.logor r (Int32.of_int r2) in
      let r = Int32.shift_left r 8 in
      let r = Int32.logor r (Int32.of_int r3) in
      r in
    let get_string () =
      let buf = Buffer.create 16 in
      let rec loop () =
        let c = get_char "get_string" in
        if c <> '\000' then (
          Buffer.add_char buf c;
          loop ()
        ) else
          Buffer.contents buf
      in
      loop () in
    let get_n_bytes n = String.init n (fun _ -> get_char "get_n_bytes") in
    let get_char () = get_char "get_char" in
    let get_many f =
      let num = get_int16 () in
      let fields = ref [] in
      for _ = 0 to num - 1 do
        fields := (f ()) :: !fields
      done;
      List.rev !fields in
    let msg =
      match typ with
      | 'R' ->
        (match get_int32 () with
         | 0l -> AuthenticationOk
         | 2l -> AuthenticationKerberosV5
         | 3l -> AuthenticationCleartextPassword
         | 4l -> AuthenticationCryptPassword (get_n_bytes 2)
         | 5l -> AuthenticationMD5Password (get_n_bytes 4)
         | 6l -> AuthenticationSCMCredential
         | _  -> UnknownMessage (typ, msg))
      | 'H' ->
        let format_code_to_format = function
          | 0 -> Text
          | 1 -> Binary
          | format_code ->
            fail_msg "Unused CopyOutResponse format: %d" format_code in
        let format_ = format_code_to_format (get_int8 ()) in
        let formats = get_many (fun () ->
          format_code_to_format (get_int16 ())) in
        CopyOutResponse (format_, formats)
      | 'd' -> CopyData (get_n_bytes len)
      | 'c' -> CopyDone
      | 'E' ->
        let acc = [| "" ; "" ; "" |] in
        let others = ref [] in
        let rec loop () =
          match get_char () with
          | '\000' ->
            { Error_response.code = acc.(0)
            ; severity = acc.(1)
            ; message = acc.(2)
            ; custom = !others }
          | 'C' -> acc.(0) <- get_string (); loop ()
          | 'S' -> acc.(1) <- get_string (); loop ()
          | 'M' -> acc.(2) <- get_string (); loop ()
          | c -> others := (c, get_string ()) :: !others ; loop () in
        ErrorResponse (loop ())
      | 'N' ->
        let strs = ref [] in
        let rec loop () =
          let field_type = get_char () in
          if field_type = '\000' then List.rev !strs (* end of list *)
          else (
            strs := (field_type, get_string ()) :: !strs;
            loop ()
          ) in
        NoticeResponse (loop ())
      | 'Z' -> ReadyForQuery (Ready.of_char (get_char ()))
      | 'K' ->
        let pid = get_int32 () in
        let key = get_int32 () in
        BackendKeyData (pid, key)
      | 'S' ->
        let param = get_string () in
        let value = get_string () in
        ParameterStatus (param, value)
      | '1' -> ParseComplete
      | '2' -> BindComplete
      | '3' -> CloseComplete
      | 'C' -> CommandComplete (get_string ())
      | 'D' ->
        DataRow (
          get_many (fun () ->
            let len = get_int32 () in
            if len < 0l then None
            else (
              if len >= 0x4000_0000l then
                fail_parse "Pgx: result field is too long";
              let len = Int32.to_int len in
              if len > Sys.max_string_length then
                fail_parse "Pgx: result field is too wide for string";
              let bytes = get_n_bytes len in
              Some bytes
            )
          )
        )
      | 'I' -> EmptyQueryResponse
      | 'n' -> NoData
      | 'T' ->
        RowDescription (
          get_many (fun () ->
            let name = get_string () in
            let table = get_int32 () in
            let col = get_int16 () in
            let oid = get_int32 () in
            let len = get_int16 () in
            let modifier = get_int32 () in
            let format = get_int16 () in
            { Row_desc.name ; table ; col ; oid ; len ; modifier ; format }
          )
        )
      | 't' -> ParameterDescription (get_many get_int32)
      | _ -> UnknownMessage (typ, msg) in
    msg
end

module Message_out = struct
  type prepare =
    { name: string
    ; query: string
    ; types: oid list }
  [@@deriving sexp]

  type portal = string [@@deriving sexp]
  type statement = string [@@deriving sexp]

  type query = string [@@deriving sexp]

  type bind =
    { portal: string
    ; name: string
    ; params: string option list }
  [@@deriving sexp]

  type startup =
    { user: string
    ; database: string }
  [@@deriving sexp]

  type t =
    | Password of string (* p *)
    | Close (* X *)
    | Sync (* S *)
    | Flush (* H *)
    | Prepare of prepare (* P *)
    | Execute of portal (* E *)
    | Bind of bind (* B *)
    | Close_statement of statement (* CP *)
    | Close_portal of portal (* CS *)
    | Describe_statement of statement (* DS *)
    | Describe_portal of portal (* DP *)
    | Startup_message of startup
    | Simple_query of query
  [@@deriving sexp]

  let add_byte buf i =
    (* Deliberately throw an exception if i isn't [0..255]. *)
    Buffer.add_char buf (Char.chr i)

  let add_int16 buf i =
    if i < 0 || i > 65_535 then
      fail_msg "Pgx: int16 %d is outside range [0..65535]." i;
    Buffer.add_char buf (Char.unsafe_chr ((i lsr 8) land 0xff));
    Buffer.add_char buf (Char.unsafe_chr (i land 0xff))

  let add_int32 buf i =
    let base = Int32.to_int i in
    let big = Int32.to_int (Int32.shift_right_logical i 24) in
    Buffer.add_char buf (Char.unsafe_chr (big land 0xff));
    Buffer.add_char buf (Char.unsafe_chr ((base lsr 16) land 0xff));
    Buffer.add_char buf (Char.unsafe_chr ((base lsr 8) land 0xff));
    Buffer.add_char buf (Char.unsafe_chr (base land 0xff))

  let check_str str =
    (* Check the string doesn't contain '\0' characters. *)
    if String.contains str '\000' then
      fail_msg "Pgx: string contains ASCII NIL character: %S" str;
    if String.length str > 0x3fff_ffff then
      fail_msg "Pgx: string is too long."

  let add_string_no_trailing_nil buf str =
    check_str str;
    Buffer.add_string buf str

  let add_string msg str =
    add_string_no_trailing_nil msg str;
    add_byte msg 0

  let str s =
    check_str s;
    s ^ "\000"

  let to_packet = function
    | Password p -> (Some 'p', str p)
    | Close -> (Some 'X', "")
    | Sync -> (Some 'S', "")
    | Flush -> (Some 'H', "")
    | Prepare { name ; query ; types } ->
      let msg = Buffer.create 128 in
      add_string msg name;
      add_string msg query;
      add_int16 msg (List.length types);
      List.iter (add_int32 msg) types;
      (Some 'P', Buffer.contents msg)
    | Execute portal ->
      let msg = Buffer.create 128 in
      add_string msg portal;
      add_int32 msg 0l; (* no limit on rows *)
      (Some 'E', Buffer.contents msg)
    | Bind { portal ; name ; params } ->
      let msg = Buffer.create 128 in
      add_string msg portal;
      add_string msg name;
      add_int16 msg 0; (* Send all parameters as text. *)
      add_int16 msg (List.length params);
      List.iter (function
        | None -> add_int32 msg 0xffff_ffffl (* NULL *)
        | Some str ->
          add_int32 msg (Int32.of_int (String.length str));
          add_string_no_trailing_nil msg str
      ) params;
      add_int16 msg 0; (* Send back all results as text. *)
      (Some 'B', Buffer.contents msg)
    | Close_statement statement ->
      (Some 'C', "S" ^ str statement)
    | Close_portal portal ->
      (Some 'C', "P" ^ str portal)
    | Describe_statement statement ->
      (Some 'D', "S" ^ str statement)
    | Describe_portal portal ->
      (Some 'D', "S" ^ str portal)
    | Startup_message { user ; database } ->
      let msg = Buffer.create 64 in
      add_int32 msg 196608l;
      add_string msg "user"; add_string msg user;
      add_string msg "database"; add_string msg database;
      add_byte msg 0;
      (None, Buffer.contents msg)
    | Simple_query q -> (Some 'Q', str q)
end

let is_first_oct_digit c = c >= '0' && c <= '3'
let is_oct_digit c = c >= '0' && c <= '7'
let oct_val c = Char.code c - 0x30

let is_hex_digit = function
  | '0'..'9'
  | 'a'..'f'
  | 'A'..'F' -> true
  | _ -> false

let hex_val c =
  let offset = match c with
    | '0'..'9' -> 0x30
    | 'a'..'f' -> 0x57
    | 'A'..'F' -> 0x37
    | _ -> failwith "hex_val"
  in Char.code c - offset

(* Deserialiser for the new 'hex' format introduced in PostgreSQL 9.0. *)
let deserialize_hex str =
  let len = String.length str in
  let buf = Buffer.create ((len-2)/2) in
  let i = ref 3 in
  while !i < len do
    let hi_nibble = str.[!i-1] in
    let lo_nibble = str.[!i] in
    i := !i+2;
    if is_hex_digit hi_nibble && is_hex_digit lo_nibble
    then begin
      let byte = ((hex_val hi_nibble) lsl 4) + (hex_val lo_nibble) in
      Buffer.add_char buf (Char.chr byte)
    end
  done;
  Buffer.contents buf

(* Deserialiser for the old 'escape' format used in PostgreSQL < 9.0. *)
let deserialize_string_escape str =
  let len = String.length str in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = str.[!i] in
    if c = '\\' then (
      incr i;
      if !i < len && str.[!i] = '\\' then (
        Buffer.add_char buf '\\';
        incr i
      ) else if !i+2 < len &&
                is_first_oct_digit str.[!i] &&
                is_oct_digit str.[!i+1] &&
                is_oct_digit str.[!i+2] then (
        let byte = oct_val str.[!i] in
        incr i;
        let byte = (byte lsl 3) + oct_val str.[!i] in
        incr i;
        let byte = (byte lsl 3) + oct_val str.[!i] in
        incr i;
        Buffer.add_char buf (Char.chr byte)
      )
    ) else (
      incr i;
      Buffer.add_char buf c
    )
  done;
  Buffer.contents buf

(* PostgreSQL 9.0 introduced the new 'hex' format for binary data.
   We must therefore check whether the data begins with a magic sequence
   that identifies this new format and if so call the appropriate parser;
   if it doesn't, then we invoke the parser for the old 'escape' format.
*)
let deserialize_string str =
  if String.starts_with str "\\x"
  then deserialize_hex str
  else deserialize_string_escape str

module Value = Pgx_value

module type IO = Io_intf.S
module type S = Pgx_intf.S

module Make (Thread : IO) = struct

  open Thread

  type conn =
    { ichan: in_channel sexp_opaque (* In_channel wrapping socket. *)
    ; chan: out_channel sexp_opaque (* Out_channel wrapping socket. *)
    ; id: int (* unique id for this connection. *)
    ; mutable in_transaction: bool
    ; verbose : int
    ; max_message_length : int
    ; mutable prepared_num : int64 (* Used to generate statement names *) }
  [@@deriving sexp]

  type t = conn Sequencer.t

  type 'a monad = 'a Thread.t

  let (>>|) x f =
    x >>= fun x -> return (f x)

  (* If true, emit a lot of debugging information about the protocol on
     stderr.*)
  let debug_protocol =
    try
      ignore (Sys.getenv "PGX_DEBUG");
      true
    with Not_found -> false

  let send_message { chan ; _ } msg =
    let (typ, msg) = Message_out.to_packet msg in
    (* Get the length in bytes. *)
    let len = 4 + String.length msg in

    (* If the length is longer than a 31 bit integer, then the message is
     * too long to send.  This limits messages to 1 GB, which should be
     * enough for anyone :-)
    *)
    if Int64.of_int len >= 0x4000_0000L then
      fail_msg "Pgx: message is larger than 1 GB";

    begin if debug_protocol then
        Thread.debug(sprintf "> %s%d %S"
                       (match typ with
                        | None -> ""
                        | Some c -> sprintf "%c " c)
                       len msg)
      else
        return ()
    end
    >>= fun () ->

    (* Write the type byte? *)
    (match typ with
     | None -> Thread.return ()
     | Some c -> output_char chan c
    ) >>= fun () ->

    (* Write the length field. *)
    output_binary_int chan len >>= fun () ->

    (* Write the buffer. *)
    output_string chan msg (* TODO possibly not write if empty *)

  (* Receive a single result message.  Parse out the message type,
   * message length, and binary message content.
  *)
  let receive_message { ichan ; chan ; max_message_length ; _ } =
    (* Flush output buffer. *)
    flush chan >>= fun () ->

    input_char ichan >>= fun typ ->
    input_binary_int ichan >>= fun len ->

    (* Discount the length word itself. *)
    let len = len - 4 in

    (* If the message is too long, give up now. *)
    if len > max_message_length then (
      (* Skip the message so we stay in synch with the stream. *)
      let bufsize = 65_536 in
      let buf = Bytes.create bufsize in
      let rec loop n =
        if n > 0 then begin
          let m = min n bufsize in
          really_input ichan buf 0 m >>= fun () ->
          loop (n - m)
        end else
          return ()
      in
      loop len >>= fun () ->

      fail_parse "Pgx: back-end message is longer than max_message_length"
    ) else (

      (* Read the binary message content. *)
      let msg = Bytes.create len in
      really_input ichan msg 0 len >>= fun () ->
      let msg = Message_in.read (typ, Bytes.to_string msg) in
      begin if debug_protocol then
          Thread.debug (sprintf "< %s" (Message_in.to_string msg))
        else return ()
      end
      >>| fun () -> msg)

  (* Send a message and expect a single result. *)
  let send_recv conn msg =
    send_message conn msg >>= fun () ->
    receive_message conn

  (* Handle an ErrorResponse anywhere, by printing and raising an exception. *)
  let pg_error ?(sync=false) ~conn fields =
    begin if Error_response.should_print fields ~verbose:conn.verbose
      then Thread.debug (Error_response.to_string ~verbose:(conn.verbose > 1) fields)
      else return ()
    end
    >>= fun () ->
    (* If conn parameter was given, then resynch - read messages until we
     * see ReadyForQuery. *)
    let rec loop () =
      receive_message conn >>= function
      | Message_in.ReadyForQuery _ -> return ()
      | _ -> loop ()
    in
    (if sync
     then send_message conn Message_out.Sync
     else return ())
    >>= loop
    >>= fun () ->
    raise (PostgreSQL_Error (Error_response.to_string fields, fields))

  let next_id = begin
    let id = ref 0 in
    (fun () ->
       (* In OCaml this doesn't allocate, and threads can't context switch except on
          allocation *)
       incr id;
       !id)
  end

  (*----- Connection. -----*)

  let connect ?host ?port ?user ?password ?database
        ?(unix_domain_socket_dir="/tmp") ?verbose ?(max_message_length=Sys.max_string_length) () =

    (* Get the username. *)
    begin match user with
    | Some user -> return user
    | None ->
      try return (Sys.getenv "PGUSER")
      with Not_found ->
        Thread.getlogin ()
    end
    >>= fun user ->

    (* Get the password. *)
    let password =
      match password with
      | Some password -> password
      | None ->
        try Sys.getenv "PGPASSWORD"
        with Not_found -> "" in

    (* Get the database name. *)
    let database =
      match database with
      | Some database -> database
      | None ->
        try Sys.getenv "PGDATABASE"
        with Not_found -> user in

    (* Get socket address using hostname and port number. *)
    let sockaddr =
      let port =
        match port with
        | Some port -> port
        | None ->
          try int_of_string (Sys.getenv "PGPORT")
          with Not_found | Failure _ -> 5432 in
      match host with
      | Some name -> Inet (name, port)
      | None ->
        try Inet (Sys.getenv "PGHOST", port)
        with Not_found -> (* use Unix domain socket. *)
          let path = sprintf "%s/.s.PGSQL.%d" unix_domain_socket_dir port in
          Unix path in

    (* Get the verbosity level *)
    let verbose =
      match verbose with
      | Some verbose -> verbose
      | None ->
        try
          Sys.getenv "PGD_PGX_VERBOSE"
          |> int_of_string
        with Not_found -> 0 in

    let id = next_id () in

    open_connection sockaddr >>= fun (ichan, chan) ->

    (* Create the connection structure. *)
    let conn =
      { ichan
      ; chan
      ; id
      ; in_transaction = false
      ; verbose
      ; max_message_length
      ; prepared_num = Int64.of_int 0 } in

    (* Send the StartUpMessage.  NB. At present we do not support SSL. *)
    let msg = Message_out.Startup_message { Message_out.user ; database } in

    (* Loop around here until the database gives a ReadyForQuery message. *)
    let rec loop msg =
      (match msg with
       | Some msg -> send_recv conn msg
       | None -> receive_message conn) >>=
      function
      | Message_in.ReadyForQuery _ -> return () (* Finished connecting! *)
      | Message_in.BackendKeyData _ ->
        (* XXX We should save this key. *)
        loop None
      | Message_in.ParameterStatus _ ->
        (* Should we do something with this? *)
        loop None
      | Message_in.AuthenticationOk -> loop None
      | Message_in.AuthenticationKerberosV5 ->
        fail_msg "Pgx: Kerberos authentication not supported"
      | Message_in.AuthenticationCleartextPassword ->
        loop (Some (Message_out.Password password))
      | Message_in.AuthenticationCryptPassword _ ->
        (* Crypt password not supported because there is no crypt(3) function
         * in OCaml.
        *)
        fail_msg "Pgx: crypt password authentication not supported"
      | Message_in.AuthenticationMD5Password salt ->
        (*    (* This is a guess at how the salt is used ... *)
              let password = salt ^ password in
              let password = Digest.string password in*)
        let digest =
          password ^ user
          |> Digest.string
          |> Digest.to_hex
          |> (fun x -> x ^ salt)
          |> Digest.string
          |> Digest.to_hex in
        let password = "md5" ^ digest in
        loop (Some (Message_out.Password password))
      | Message_in.AuthenticationSCMCredential ->
        fail_msg "Pgx: SCM Credential authentication not supported"
      | Message_in.ErrorResponse err ->
        pg_error ~conn err
      | Message_in.NoticeResponse _ ->
        (* XXX Do or print something here? *)
        loop None
      | _ ->
        (* Silently ignore unknown or unexpected message types. *)
        loop None
    in
    loop (Some msg)
    >>| fun () ->
    Sequencer.create conn

  let close seq =
    Sequencer.enqueue seq (fun conn ->
      (* Be nice and send the terminate message. *)
      send_message conn Message_out.Close >>= fun () ->
      flush conn.chan >>= fun () ->
      (* Closes the underlying socket too. *)
      close_in conn.ichan)

  let with_conn ?host ?port ?user ?password ?database ?unix_domain_socket_dir
        ?verbose ?max_message_length f =
    connect ?host ?port ?user ?password ?database ?unix_domain_socket_dir
      ?verbose ?max_message_length ()
    >>= fun dbh ->
    protect (fun () -> f dbh)
      ~finally:(fun () -> close dbh)

  let sync conn =
    send_message conn Message_out.Sync >>= fun () ->
    let rec loop () =
      receive_message conn >>= function
      | Message_in.ReadyForQuery _ -> return () (* Finished! *)
      | Message_in.ErrorResponse err -> pg_error ~conn err (* Error *)
      | _ -> loop () in
    loop ()

  let ping seq =
    Sequencer.enqueue seq (fun conn ->
      sync conn)

  let alive conn =
    catch
      (fun () -> ping conn >>= fun () -> return true)
      (fun _ -> return false)

  type param = string option [@@deriving sexp]
  type result = string option [@@deriving sexp]
  type row = result list [@@deriving sexp]

  let flush_msg conn =
    send_message conn Message_out.Flush >>= fun () ->
    (* Might as well actually flush the channel too, otherwise what is the
     * point of executing this command?
    *)
    flush conn.chan

  module Prepared = struct
    type s =
      { conn : conn Sequencer.t sexp_opaque
      ; name : string }
    [@@deriving sexp_of]

    let prepare ?name ?(types = []) seq ~query =
      Sequencer.enqueue seq (fun conn ->
        let name = match name with
          | Some name -> name
          | None ->
            let n = conn.prepared_num in
            conn.prepared_num <- Int64.succ n;
            sprintf "pgx_prepared_%Ld" n
        in
        send_message conn
          (Message_out.Prepare { Message_out.name ; query ; types })
        >>= fun () ->
        flush_msg conn >>= fun () ->
        let rec loop () =
          receive_message conn >>= function
          | Message_in.ErrorResponse err -> pg_error ~sync:true ~conn err
          | Message_in.ParseComplete ->
            (* Finished! *)
            return { conn = seq ; name }
          | Message_in.NoticeResponse _ ->
            (* XXX Do or print something here? *)
            loop ()
          | msg ->
            fail_msg "Pgx: unknown response from parse: %s"
              (Message_in.to_string msg)
        in
        loop ())

    let close { conn ; name } =
      Sequencer.enqueue conn (fun conn ->
        send_message conn (Message_out.Close_statement name) >>= fun () ->
        flush_msg conn >>= fun () ->
        let rec loop () =
          receive_message conn
          >>= function
          | Message_in.ErrorResponse err -> pg_error ~conn err
          | Message_in.CloseComplete -> return () (* Finished! *)
          | Message_in.NoticeResponse _ ->
            (* XXX Do or print something here? *)
            loop ()
          | m ->
            fail_msg "Pgx: unknown response from close: %s"
              (Message_in.to_string m)
        in
        loop ())

    let with_prepare ?name ?types t ~query ~f =
      prepare ?name ?types t ~query
      >>= fun s ->
      protect
        (fun () -> f s)
        ~finally:(fun () ->
          close s)

    let execute_iter ?(portal = "") { name ; conn }
          ~params ~f =
      let encode_unprintable b =
        let len = String.length b in
        let buf = Buffer.create (len * 2) in
        for i = 0 to len - 1 do
          let c = b.[i] in
          let cc = Char.code c in
          if cc < 0x20 || cc > 0x7e then
            Buffer.add_string buf (sprintf "\\%03o" cc) (* non-print -> \ooo *)
          else if c = '\\' then
            Buffer.add_string buf "\\\\" (* \ -> \\ *)
          else
            Buffer.add_char buf c
        done;
        Buffer.contents buf
      in
      let params =
        List.map (fun s ->
          Value.to_string s
          |> Option.map encode_unprintable)
          params
      in
      Sequencer.enqueue conn (fun conn ->
        send_message conn
          (Message_out.Bind { Message_out.portal ; name ; params })
        >>= fun () ->
        send_message conn (Message_out.Execute portal) >>= fun () ->
        send_message conn Message_out.Sync >>= fun () ->

        (* Process the message(s) received from the database until we read
         * ReadyForQuery.  In the process we may get some rows back from
         * the database, no data, or an error.
        *)
        let rec loop () =
          (* NB: receive_message flushes the output connection. *)
          receive_message conn >>= function
          | Message_in.ReadyForQuery _ -> return () (* Finished! *)
          | Message_in.ErrorResponse err -> pg_error ~conn err (* Error *)
          | Message_in.NoticeResponse _ ->
            (* XXX Do or print something here? *)
            loop ()
          | Message_in.BindComplete -> loop ()
          | Message_in.CommandComplete _ -> loop ()
          | Message_in.EmptyQueryResponse -> loop ()
          | Message_in.DataRow fields ->
            List.map (Option.bind (fun v ->
              deserialize_string v
              |> Value.of_string))
              fields
            |> f
            >>= loop
          | Message_in.NoData -> loop ()
          | Message_in.ParameterStatus _ ->
            (* 43.2.6: ParameterStatus messages will be generated whenever
             * the active value changes for any of the parameters the backend
             * believes the frontend should know about. Most commonly this
             * occurs in response to a SET SQL command executed by the
             * frontend, and this case is effectively synchronous -- but it
             * is also possible for parameter status changes to occur because
             * the administrator changed a configuration file and then sent
             * the SIGHUP signal to the postmaster.
            *)
            loop ()
          | Message_in.CopyOutResponse (format_, format_list) ->
            (match format_ with
             | Message_in.Text ->
               List.iter (function
                 | Message_in.Binary ->
                   fail_msg "Pgx.query: Binary column found in text CopyOutResponse"
                 | _ -> ()) format_list;
               loop ()
             | Message_in.Binary ->
               fail_msg "Pgx.iter_execute: CopyOutResponse for binary is \
                         not implemented yet")
          | Message_in.CopyData row ->
            f [row |> deserialize_string |> Value.of_string]
            >>= fun () -> loop ()
          | Message_in.CopyDone -> loop ()
          | m ->
            fail_msg "Pgx: unknown response message: %s"
              (Message_in.to_string m)
        in
        loop ())

    let execute_fold ?portal s ~params ~init ~f =
      let acc = ref init in
      execute_iter ?portal s ~params
        ~f:(fun fields ->
          f !acc fields
          >>| fun res ->
          acc := res)
      >>| fun () ->
      !acc

    let execute_map ?portal s ~params ~f =
      execute_fold ?portal s ~params ~init:[] ~f:(fun acc row ->
        f row
        >>| fun res ->
        res :: acc)
      >>| List.rev

    let execute ?portal s ~params =
      execute_map s ?portal ~params ~f:return

    let execute_many s ~params =
      List.fold_left (fun acc params ->
        acc
        >>= fun acc -> execute s ~params
        >>| fun results -> results :: acc)
        (return []) params
      >>| List.rev

    let describe { conn ; name } =
      Sequencer.enqueue conn (fun conn ->
        send_message conn (Message_out.Describe_statement name) >>= fun () ->
        flush_msg conn >>= fun () ->
        receive_message conn >>= (function
          | Message_in.ErrorResponse err -> pg_error ~sync:true ~conn err
          | Message_in.ParameterDescription params -> return params
          | msg ->
            fail_msg "Pgx: unknown response from describe: %s"
              (Message_in.to_string msg)) >>= fun params ->
        receive_message conn >>= function
        | Message_in.ErrorResponse err -> pg_error ~sync:true ~conn err
        | Message_in.NoData -> return (params, None)
        | Message_in.RowDescription fields ->
          let fields = List.map Result_desc.of_row_desc fields in
          return (params, Some fields)
        | msg ->
          fail_msg "Pgx: unknown response from describe: %s"
            (Message_in.to_string msg))

    let close_portal ?(portal = "") { conn ; _ } =
      Sequencer.enqueue conn (fun conn ->
        send_message conn (Message_out.Close_portal portal) >>= fun () ->
        flush_msg conn >>= fun () ->
        let rec loop () =
          receive_message conn >>= function
          | Message_in.ErrorResponse err -> pg_error ~conn err
          | Message_in.CloseComplete -> return ()
          | Message_in.NoticeResponse _ ->
            (* XXX Do or print something here? *)
            loop ()
          | msg ->
            fail_msg "Pgx: unknown response from close: %s"
              (Message_in.to_string msg)
        in
        loop ())

    let describe_portal ?(portal = "") { conn ; _ } =
      Sequencer.enqueue conn (fun conn ->
        send_message conn (Message_out.Describe_portal portal) >>= fun () ->
        flush_msg conn >>= fun () ->
        receive_message conn >>= function
        | Message_in.ErrorResponse err -> pg_error ~sync:true ~conn err
        | Message_in.NoData -> return None
        | Message_in.RowDescription fields ->
          let fields = List.map Result_desc.of_row_desc fields in
          return (Some fields)
        | msg ->
          fail_msg "Pgx: unknown response from describe: %s"
            (Message_in.to_string msg))
  end

  let simple_query' dbh query =
    send_message dbh (Message_out.Simple_query query) >>= fun () ->
    let rec loop acc rows state =
      receive_message dbh >>= fun msg ->
      match state, msg with
      | _, Message_in.EmptyQueryResponse ->
        (match acc, rows with
         | [], [] -> return []
         | _ -> fail_msg "Pgx.query: EmptyQueryResponse with rows")
      | _, Message_in.CopyOutResponse (format_, format_list) ->
        (match format_ with
         | Message_in.Text ->
           List.iter (function
             | Message_in.Binary ->
               fail_msg "Pgx.query: Binary column found in text CopyOutResponse"
             | _ -> ()) format_list;
           loop acc rows state
         | Message_in.Binary ->
           fail_msg "Pgx.query: CopyOutResponse for binary is not implemented yet")
      | _, Message_in.CopyData row ->
        loop acc ([row |> deserialize_string |> Value.of_string]::rows) state
      | _, Message_in.CopyDone ->
        loop acc rows state
      | `Rows, Message_in.DataRow row ->
        let row =
          List.map (Option.bind (fun v ->
            deserialize_string v
            |> Value.of_string))
            row
        in
        loop acc (row::rows) `Rows
      | (`Row_desc |  `Rows ), Message_in.CommandComplete _ ->
        let rows = List.rev rows in
        loop (rows::acc) [] `Row_desc
      | `Row_desc, Message_in.RowDescription _ -> loop acc rows `Rows
      | _, Message_in.ReadyForQuery _ ->
        (match rows with
         | [] -> return (List.rev acc)
         | _ -> fail_msg "Pgx.query: unused rows for acc")
      | _, Message_in.ErrorResponse err -> pg_error ~conn:dbh err
      (* XXX log this notice properly *)
      | _, Message_in.NoticeResponse _ -> loop acc rows state
      (* The query changed a setting *)
      | _, Message_in.ParameterStatus _ -> loop acc rows state
      | _, msg -> fail_msg "Pgx.query: unknown response message: %s"
                    (Message_in.to_string msg) in
    loop [] [] `Row_desc

  let simple_query seq query =
    Sequencer.enqueue seq (fun dbh ->
      simple_query' dbh query)

  let execute ?(params=[]) db query =
    match params with
    | [] ->
      simple_query db query
      >>| (function
        | rows :: [] -> rows
        | results ->
          fail_msg "Pgx.execute: Query returned %d result sets but \
                    execute should only ever return one. Query was: %s"
            (List.length results) query)
    | _ ->
      Prepared.(with_prepare db ~query ~f:(fun s ->
        execute s ~params))

  let execute_iter ?(params=[]) db query ~f =
    Prepared.(with_prepare db ~query ~f:(fun s ->
      execute_iter s ~params ~f))

  let execute_fold ?(params=[]) db query ~init ~f =
    Prepared.(with_prepare db ~query ~f:(fun s ->
      execute_fold s ~params ~init ~f))

  let execute_map ?(params=[]) db query ~f =
    Prepared.(with_prepare db ~query ~f:(fun s ->
      execute_map s ~params ~f))

  let begin_work ?isolation ?access ?deferrable seq =
    Sequencer.enqueue seq (fun conn ->
      if conn.in_transaction
      then invalid_arg "begin_work: cannot transact while in another transaction"
      else conn.in_transaction <- true;
      let isolation_str = match isolation with
        | None -> ""
        | Some x -> " isolation level " ^ (Isolation.to_string x) in
      let access_str = match access with
        | None -> ""
        | Some x -> " " ^ (Access.to_string x) in
      let deferrable_str = match deferrable with
        | None -> ""
        | Some true -> " deferrable"
        | Some false -> " not deferrable" in
      let query = "begin work" ^ isolation_str ^ access_str ^ deferrable_str in
      simple_query' conn query)
    >>| fun _ -> seq

  let commit seq =
    Sequencer.enqueue seq (fun conn ->
      if not conn.in_transaction then
        invalid_arg "commit: cannot run outside of transaction";
      simple_query' conn "commit"
      >>| fun _ ->
      conn.in_transaction <- false)

  let rollback seq =
    Sequencer.enqueue seq (fun conn ->
      if not conn.in_transaction then
        invalid_arg "rollback: cannot run outside of transaction";
      simple_query' conn "rollback"
      >>| fun _ ->
      conn.in_transaction <- false)

  let with_transaction ?isolation ?access ?deferrable conn f =
    begin_work ?isolation ?access ?deferrable conn
    >>= fun conn ->
    catch
      (fun () ->
         f conn >>= fun r ->
         commit conn >>= fun () ->
         return r
      )
      (fun e ->
         let backtrace = Printexc.get_raw_backtrace () in
         rollback conn >>= fun () ->
         Printexc.raise_with_backtrace e backtrace
      )

  let execute_many conn ~query ~params =
    Prepared.(with_prepare conn ~query ~f:(fun s ->
      execute_many s ~params))
end
