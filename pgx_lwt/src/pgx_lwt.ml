open Lwt

module Thread = struct
  type 'a t = 'a Lwt.t

  let return = return
  let (>>=) = (>>=)

  let catch = catch

  type sockaddr =
    | Unix of string
    | Inet of string * int

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let sexp_of_in_channel _ = Sexplib0.Sexp.Atom "<opaque>"
  let sexp_of_out_channel _ = Sexplib0.Sexp.Atom "<opaque>"

  let output_char = Lwt_io.write_char
  let output_string = Lwt_io.write
  let output_binary_int w n =
    let chr = Char.chr in
    output_char w (chr (n lsr 24)) >>= fun () ->
    output_char w (chr ((n lsr 16) land 255)) >>= fun () ->
    output_char w (chr ((n lsr 8) land 255)) >>= fun () ->
    output_char w (chr (n land 255))
  let flush = Lwt_io.flush

  let input_char = Lwt_io.read_char

  let really_input = Lwt_io.read_into_exactly

  let input_binary_int r =
    let b = Bytes.create 4 in
    Lwt_io.read_into_exactly r b 0 4
    >|= fun () ->
    let s = Bytes.to_string b in
    let code = Char.code in
    (code s.[0] lsl 24) lor (code s.[1] lsl 16)
    lor (code s.[2] lsl 8) lor (code s.[3])

  let close_in = Lwt_io.close

  let open_connection sockaddr =
    (match sockaddr with
     | Unix path -> return (Unix.ADDR_UNIX path)
     | Inet (hostname, port) ->
       Lwt_unix.gethostbyname hostname
       >|= fun { Lwt_unix.h_addr_list ; _ } ->
       let len = Array.length h_addr_list in
       let i = Random.int len in
       let addr = h_addr_list.(i) in
       Unix.ADDR_INET (addr, port))
    >>= Lwt_io.open_connection

  (* The unix getlogin syscall can fail *)
  let getlogin () =
    Unix.getuid ()
    |> Lwt_unix.getpwuid
    >|= fun { Lwt_unix.pw_name ; _ } -> pw_name

  let debug = Lwt_io.eprintl

  let protect f ~finally =
    Lwt.finalize f finally

  module Sequencer = struct
    type 'a monad = 'a t
    type 'a t = 'a * Lwt_mutex.t
    let sexp_of_t _ = Sexplib0.Sexp.Atom "<opaque>"

    let create t =
      t, Lwt_mutex.create ()

    let enqueue (t, mutex) f =
      Lwt_mutex.with_lock mutex (fun () ->
        f t)

  end
end

include (Pgx.Make(Thread))
