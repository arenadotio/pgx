open Lwt

module Thread : Pgx_lwt.Io_intf.S = struct
  type sockaddr =
    | Unix of string
    | Inet of string * int

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let output_char = Lwt_io.write_char
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let close_in = Lwt_io.close

  (* The unix getlogin syscall can fail *)
  let getlogin () =
    Unix.getuid () |> Lwt_unix.getpwuid >|= fun { Lwt_unix.pw_name; _ } -> pw_name
  ;;

  let open_connection sockaddr =
    (match sockaddr with
    | Unix path -> return (Unix.ADDR_UNIX path)
    | Inet (hostname, port) ->
      Lwt_unix.gethostbyname hostname
      >|= fun { Lwt_unix.h_addr_list; _ } ->
      let len = Array.length h_addr_list in
      let i = Random.int len in
      let addr = h_addr_list.(i) in
      Unix.ADDR_INET (addr, port))
    >>= Lwt_io.open_connection
  ;;
end

include Pgx_lwt.Make (Thread)
