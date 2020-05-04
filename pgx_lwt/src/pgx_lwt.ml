module IO_intf = Io_intf

module type S = Pgx.S with type 'a IO.t = 'a Lwt.t

module Thread = struct
  open Lwt

  module Make (IO : IO_intf.S) = struct
    type 'a t = 'a Lwt.t

    let return = return
    let ( >>= ) = ( >>= )
    let catch = catch

    type sockaddr = IO.sockaddr =
      | Unix of string
      | Inet of string * int

    type in_channel = IO.in_channel
    type out_channel = IO.out_channel

    let output_char = IO.output_char
    let output_string = IO.output_string

    let output_binary_int w n =
      let chr = Char.chr in
      output_char w (chr (n lsr 24))
      >>= fun () ->
      output_char w (chr ((n lsr 16) land 255))
      >>= fun () ->
      output_char w (chr ((n lsr 8) land 255))
      >>= fun () -> output_char w (chr (n land 255))
    ;;

    let flush = IO.flush
    let input_char = IO.input_char
    let really_input = IO.really_input

    let input_binary_int r =
      let b = Bytes.create 4 in
      really_input r b 0 4
      >|= fun () ->
      let s = Bytes.to_string b in
      let code = Char.code in
      (code s.[0] lsl 24) lor (code s.[1] lsl 16) lor (code s.[2] lsl 8) lor code s.[3]
    ;;

    let close_in = IO.close_in
    let open_connection = IO.open_connection
    let getlogin = IO.getlogin
    let debug s = Logs_lwt.debug (fun m -> m "%s" s)
    let protect f ~finally = Lwt.finalize f finally

    module Sequencer = struct
      type 'a monad = 'a t
      type 'a t = 'a * Lwt_mutex.t

      let create t = t, Lwt_mutex.create ()
      let enqueue (t, mutex) f = Lwt_mutex.with_lock mutex (fun () -> f t)
    end
  end
end

module Make (IO : IO_intf.S) = struct
  module Thread = Thread.Make (IO)
  include Pgx.Make (Thread)
end
