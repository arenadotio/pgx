module Io_intf = Io_intf

module type S = Pgx.S with type 'a Io.t = 'a Lwt.t

module Thread = struct
  open Lwt

  module Make (Io : Io_intf.S) = struct
    type 'a t = 'a Lwt.t

    let return = return
    let ( >>= ) = ( >>= )
    let catch = catch

    type sockaddr = Io.sockaddr =
      | Unix of string
      | Inet of string * int

    type in_channel = Io.in_channel
    type out_channel = Io.out_channel

    let output_char = Io.output_char
    let output_string = Io.output_string

    let output_binary_int w n =
      let chr = Char.chr in
      output_char w (chr (n lsr 24))
      >>= fun () ->
      output_char w (chr ((n lsr 16) land 255))
      >>= fun () ->
      output_char w (chr ((n lsr 8) land 255))
      >>= fun () -> output_char w (chr (n land 255))
    ;;

    let flush = Io.flush
    let input_char = Io.input_char
    let really_input = Io.really_input

    let input_binary_int r =
      let b = Bytes.create 4 in
      really_input r b 0 4
      >|= fun () ->
      let s = Bytes.to_string b in
      let code = Char.code in
      (code s.[0] lsl 24) lor (code s.[1] lsl 16) lor (code s.[2] lsl 8) lor code s.[3]
    ;;

    let close_in = Io.close_in
    let open_connection = Io.open_connection
    let upgrade_ssl = `Not_supported
    let getlogin = Io.getlogin
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

module Make (Io : Io_intf.S) = struct
  module Thread = Thread.Make (Io)
  include Pgx.Make (Thread)
end
