(* Draft for cherry-picking into pgx.
 *
 * Copyright (C) 2020 Petter A. Urkedal
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
open Lwt.Syntax
module Channel = Mirage_channel.Make (Conduit_mirage.Flow)

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

module Make
    (RANDOM : Mirage_random.S)
    (MCLOCK : Mirage_clock.MCLOCK)
    (STACK : Mirage_stack.V4) =
struct
  module Dns = Dns_client_mirage.Make (RANDOM) (MCLOCK) (STACK)

  type sockaddr = Thread.sockaddr =
    | Unix of string
    | Inet of string * int

  let connect_stack stack sockaddr =
    let dns = Dns.create stack in
    let* conduit = Conduit_mirage.(with_tcp empty (stackv4 (module STACK)) stack) in
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
    let+ flow = Conduit_mirage.connect conduit client in
    let ch = Channel.create flow in
    ch, ch
  ;;

  let create stack =
    let open_connection = connect_stack stack in
    (module struct
      module T = struct
        include Thread

        let open_connection = open_connection
      end

      include Pgx_lwt.Make (T)
    end : Pgx_lwt.S.Pgx_impl)
  ;;
end
