(* Copyright (C) 2020 Petter A. Urkedal
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml static compilation exception.
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

open Lwt.Infix

(* Defining this inline so we can use older lwt versions. *)
let ( let* ) = Lwt.bind
let ( let+ ) t f = Lwt.map f t

module Make
    (RANDOM : Mirage_random.S)
    (TIME : Mirage_time.S)
    (MCLOCK : Mirage_clock.MCLOCK)
    (PCLOCK : Mirage_clock.PCLOCK)
    (STACK : Tcpip.Stack.V4V6) =
struct
  module Channel = Mirage_channel.Make (STACK.TCP)

  module Thread = struct
    type sockaddr =
      | Unix of string
      | Inet of string * int

    type in_channel = Channel.t
    type out_channel = Channel.t

    let output_char oc c =
      Channel.write_char oc c;
      Lwt.return_unit
    ;;

    let output_string oc s =
      Channel.write_string oc s 0 (String.length s);
      Lwt.return_unit
    ;;

    let flush oc =
      Channel.flush oc
      >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)
    ;;

    let input_char ic =
      Channel.read_char ic
      >>= function
      | Ok (`Data c) -> Lwt.return c
      | Ok `Eof -> Lwt.fail End_of_file
      | Error err -> Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)
    ;;

    let really_input ic buf off len =
      Channel.read_exactly ~len ic
      >>= function
      | Ok (`Data bufs) ->
        let content = Cstruct.copyv bufs in
        Bytes.blit_string content 0 buf off len;
        Lwt.return_unit
      | Ok `Eof -> Lwt.fail End_of_file
      | Error err -> Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)
    ;;

    let close_in oc =
      Channel.close oc
      >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)
    ;;

    let getlogin () = Lwt.fail_with "Running under MirageOS. getlogin not available."
  end

  module Dns = Dns_client_mirage.Make (RANDOM) (TIME) (MCLOCK) (PCLOCK) (STACK)

  type sockaddr = Thread.sockaddr =
    | Unix of string
    | Inet of string * int

  module TCP = Conduit_mirage.TCP (STACK)

  let connect_stack stack sockaddr =
    let dns = Dns.create stack in
    let* client =
      match sockaddr with
      | Unix _ -> Lwt.fail_with "Running under MirageOS. Unix sockets are not available."
      | Inet (host, port) ->
        (match Ipaddr.of_string host with
        | Ok ipaddr -> Lwt.return (`TCP (ipaddr, port))
        | Error _ ->
          let host' = host |> Domain_name.of_string_exn |> Domain_name.host_exn in
          Dns.gethostbyname dns host'
          >>= (function
          | Ok ipaddr -> Lwt.return (`TCP (Ipaddr.V4 ipaddr, port))
          | Error (`Msg msg) -> Lwt.fail_with msg))
    in
    let+ flow = TCP.connect stack client in
    let ch = Channel.create flow in
    ch, ch
  ;;

  let connect stack =
    let open_connection = connect_stack stack in
    (module struct
      module T : Pgx_lwt.Io_intf.S = struct
        include Thread

        let open_connection = open_connection
      end

      include Pgx_lwt.Make (T)
    end : Pgx_lwt.S)
  ;;
end
