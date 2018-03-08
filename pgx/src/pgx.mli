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
module type IO = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  type in_channel
  type out_channel
  type sockaddr =
    | Unix of string (* socket address in the unix domain *)
    | Inet of string * int (* host, port *)
  val open_connection : sockaddr -> (in_channel * out_channel) t
  val output_char : out_channel -> char -> unit t
  val output_binary_int : out_channel -> int -> unit t
  val output_string : out_channel -> string -> unit t
  val flush : out_channel -> unit t
  val input_char : in_channel -> char t
  val input_binary_int : in_channel -> int t
  val really_input : in_channel -> Bytes.t -> int -> int -> unit t
  val close_in : in_channel -> unit t
  val getlogin : unit -> string t
  val debug : string -> unit t
  val protect :  (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

  (* Used to prevent multiple concurrent queries *)
  module Sequencer : sig
    type 'a monad = 'a t
    type 'a t

    val create : 'a -> 'a t
    val enqueue : 'a t -> ('a -> 'b monad) -> 'b monad
  end
end

type oid = int32 [@@deriving sexp]

module Isolation : sig
  type t =
    | Serializable
    | Repeatable_read
    | Read_committed
    | Read_uncommitted
  [@@deriving sexp]
end

module Access : sig
  type t = Read_write | Read_only [@@deriving sexp]
end

module Result_desc : sig
  type t =
    { name : string   (** Field name. *)
    ; table : oid option   (** OID of table. *)
    ; column : int option   (** Column number of field in table. *)
    ; field_type : oid   (** The type of the field. *)
    ; length : int    (** Length of the field. *)
    ; modifier : int32 }   (** Type modifier. *)
  [@@deriving sexp]
end

module Value = Pgx_value

type param = Value.t [@@deriving sexp_of] (** None is NULL. *)
type result = Value.t [@@deriving sexp_of] (** None is NULL. *)
type row = Value.t list [@@deriving sexp_of] (** One row is a list of fields. *)

type params_description = oid list [@@deriving sexp]

module Error_response : sig
  type t [@@deriving sexp]
end

exception PostgreSQL_Error of string * Error_response.t [@@deriving sexp]
(** For errors generated by the PostgreSQL database back-end.  The
 * first argument is a printable error message.  The second argument
 * is the complete set of error fields returned from the back-end.
 * See [http://www.postgresql.org/docs/8.1/static/protocol-error-fields.html] *)

module type S = sig
  type t

  type 'a monad

  val connect
    : ?host:string
    -> ?port:int
    -> ?user:string
    -> ?password:string
    -> ?database:string
    -> ?unix_domain_socket_dir:string
    -> ?verbose:int
    -> ?max_message_length:int
    -> unit
    -> t monad
  (** Connect to the database.  The normal [$PGDATABASE], etc. environment
      variables are available.

      [max_message_length] is the maximum message length accepted from the back-end.
      The default is [Sys.max_string_length], which means that we will try to
      read as much data from the back-end as we can, and this may cause us to
      run out of memory (particularly on 64 bit machines), causing a
      possible denial of service.  You may want to set this to a smaller
      size to avoid this happening. *)

  val close : t -> unit monad
  (** Close the database handle.  You must call this after you have
      finished with the handle, or else you will get leaked file
      descriptors. *)

  val with_conn
    : ?host:string
    -> ?port:int
    -> ?user:string
    -> ?password:string
    -> ?database:string
    -> ?unix_domain_socket_dir:string
    -> ?verbose:int
    -> ?max_message_length:int
    -> (t -> 'a monad)
    -> 'a monad
  (** Calls [connect], passes the DB handle to the callback, then calls
      [close]. This is the preferred way to use this library since it cleans up
      after itself. *)

  val ping : t -> unit monad
  (** Ping the database.  If the database is not available, some sort of
      exception will be thrown. *)

  val alive : t -> bool monad
  (** This function is a wrapper of [ping] that returns a boolean instead of
      raising an exception. *)

  val begin_work
    : ?isolation:Isolation.t
    -> ?access:Access.t
    -> ?deferrable:bool
    -> t
    -> t monad
  (** Start a transaction. *)

  val commit : t -> unit monad
  (** Commit a transaction. Throws an exception if no transaction is open.
      Use [with_transaction] when possible. *)

  val rollback : t -> unit monad
  (** Rollback a transaction. Throws an exception if no transaction is open.
      Use [with_transaction] when possible. *)

  val with_transaction
    : ?isolation:Isolation.t
    -> ?access:Access.t
    -> ?deferrable:bool
    -> t
    -> (t -> 'b monad)
    -> 'b monad
  (** [with_transaction db ?isolation ?access ?deferrable f] wraps your
      function [f] inside a transactional block.
      See [begin_work] for a description of [isolation], [access], and
      [deferrable].
      If [f] throws an exception, the transaction will be rolled back. Otherwise
      the transaction will be commited. It is an error to call [commit] or
      [rollback] manually inside of this function. *)

  module Prepared : sig
    type s [@@deriving sexp_of]

    val prepare
      : ?name:string
      -> ?types:oid list
      -> t
      -> query:string
      -> s monad
    (** [prepare ?name ?types conn ~query] prepares the statement [query] and
        sets the parameter types to [types].
        If no [name] is given, a random name will be generated.
        If no types are given, then the PostgreSQL engine infers types. *)

    val close : s -> unit monad
    (** [close_statement t] closes a prepared statement and frees
        up any resources. *)

    val with_prepare
      : ?name:string
      -> ?types:oid list
      -> t
      -> query:string
      -> f:(s -> 'a monad)
      -> 'a monad
    (** [prepare] a query, execute [f], and then [close_statement] *)

    val execute
      : ?portal:string
      -> s
      -> params:param list
      -> row list monad
    (** [execute conn ~params t] executes the given prepared statement, with
        the given parameters [params], returning the result rows (if any).

        There are several steps involved at the protocol layer:
        (1) a "portal" is created from the statement, binding the
        parameters in the statement (Bind).
        (2) the portal is executed (Execute).
        (3) we synchronise the connection (Sync).

        The optional [?portal] parameter may be used to name the portal
        created in step (1) above (otherwise the unnamed portal is used).
        This is only important if you want to call {!describe_portal}
        to find out the result types. *)

    val execute_fold
      : ?portal:string
      -> s
      -> params:param list
      -> init:'accum
      -> f:('accum -> row -> 'accum monad)
      -> 'accum monad

    val execute_iter
      : ?portal:string
      -> s
      -> params:param list
      -> f:(row -> unit monad)
      -> unit monad

    val execute_many
      : s
      -> params:param list list
      -> row list list monad

    val describe
      : s
      -> (params_description * Result_desc.t list option) monad
    (** [describe_statement t] describes the statement's parameter types and
        result types. *)

    val close_portal : ?portal:string -> s -> unit monad
    (** [close_portal conn ?portal ()] closes a portal and frees up any
        resources. *)

    val describe_portal
      : ?portal:string
      -> s
      -> Result_desc.t list option monad
      (** [describe_portal conn ?portal ()] describes the named or unnamed
          portal's result types. *)
  end

  val execute : ?params:row -> t -> string -> row list monad
  (** [execute conn ?params query] prepares and executes the statement
      [query] and returns the result. *)

  val execute_fold
    : ?params:param list
    -> t
    -> string
    -> init:'accum
    -> f:('accum -> row -> 'accum monad)
    -> 'accum monad

  val execute_iter
    : ?params:param list
    -> t
    -> string
    -> f:(row -> unit monad)
    -> unit monad

  val execute_many
    : t
    -> query:string
    -> params:param list list
    -> row list list monad
  (** Prepares a query as in [execute] and then executes it once per set of
      parameters in [params]. This is more efficient than calling [execute]
      in a loop because the query is only prepared once. *)

  val simple_query : t -> string -> row list list monad
  (** [simple_query conn query] executes the command(s) in the given [query]
      and returns a list of query results (i.e. if you run two queries, you
      will get a list with two elements: the results of the first query
      followed by the results of the second query. *)
end

module Make : functor (Thread : IO) -> S with type 'a monad = 'a Thread.t
