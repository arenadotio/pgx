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

external reraise : exn -> _ = "%reraise"

module Simple_thread = struct
  type 'a t = 'a

  let return x = x
  let ( >>= ) v f = f v

  let catch f fexn =
    try f () with
    | e -> fexn e
  ;;

  type sockaddr =
    | Unix of string
    | Inet of string * int

  type nonrec in_channel = in_channel
  type nonrec out_channel = out_channel

  let open_connection sockaddr =
    let std_socket =
      match sockaddr with
      | Unix path -> Unix.ADDR_UNIX path
      | Inet (hostname, port) ->
        let hostent = Unix.gethostbyname hostname in
        (* Choose a random address from the list. *)
        let addrs = hostent.Unix.h_addr_list in
        let len = Array.length addrs in
        let i = Random.int len in
        let addr = addrs.(i) in
        Unix.ADDR_INET (addr, port)
    in
    Unix.open_connection std_socket
  ;;

  let upgrade_ssl = `Not_supported

  let output_char = output_char
  let output_binary_int = output_binary_int
  let output_string = output_string
  let flush = flush
  let input_char = input_char
  let input_binary_int = input_binary_int
  let really_input = really_input
  let close_in = close_in

  (* The unix getlogin syscall can fail *)
  let getlogin () = Unix.getuid () |> Unix.getpwuid |> fun { Unix.pw_name; _ } -> pw_name
  let debug = prerr_endline

  let protect f ~(finally : unit -> unit) =
    let result = ref None in
    try
      result := Some (f ());
      raise Exit
    with
    | Exit as e ->
      finally ();
      (match !result with
      | Some x -> x
      | None -> reraise e)
    | e ->
      finally ();
      reraise e
  ;;

  module Sequencer = struct
    type 'a monad = 'a t
    type 'a t = 'a

    let create t = t
    let enqueue t f = f t
  end
end

module M = Pgx.Make (Simple_thread)
include M
