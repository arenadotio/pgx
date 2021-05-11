open Types

module type S = sig
  type t

  module Io : sig
    type 'a t
    type ssl_config

    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    val protect : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t
  end

  (** Connect to the database.  The normal [$PGDATABASE], etc. environment
      variables are available.

      [max_message_length] is the maximum message length accepted from the back-end.
      The default is [Sys.max_string_length], which means that we will try to
      read as much data from the back-end as we can, and this may cause us to
      run out of memory (particularly on 64 bit machines), causing a
      possible denial of service.  You may want to set this to a smaller
      size to avoid this happening. *)
  val connect
    :  ?ssl:[ `Auto | `No | `Always of Io.ssl_config ]
    -> ?host:string
    -> ?port:int
    -> ?user:string
    -> ?password:string
    -> ?database:string
    -> ?unix_domain_socket_dir:string
    -> ?verbose:int
    -> ?max_message_length:int
    -> unit
    -> t Io.t

  (** Close the database handle.  You must call this after you have
      finished with the handle, or else you will get leaked file
      descriptors. *)
  val close : t -> unit Io.t

  (** Calls [connect], passes the DB handle to the callback, then calls
      [close]. This is the preferred way to use this library since it cleans up
      after itself. *)
  val with_conn
    :  ?ssl:[ `Auto | `No | `Always of Io.ssl_config ]
    -> ?host:string
    -> ?port:int
    -> ?user:string
    -> ?password:string
    -> ?database:string
    -> ?unix_domain_socket_dir:string
    -> ?verbose:int
    -> ?max_message_length:int
    -> (t -> 'a Io.t)
    -> 'a Io.t

  (** Ping the database.  If the database is not available, some sort of
      exception will be thrown. *)
  val ping : t -> unit Io.t

  (** This function is a wrapper of [ping] that returns a boolean instead of
      raising an exception. *)
  val alive : t -> bool Io.t

  (** Start a transaction. *)
  val begin_work
    :  ?isolation:Isolation.t
    -> ?access:Access.t
    -> ?deferrable:bool
    -> t
    -> t Io.t

  (** Commit a transaction. Throws an exception if no transaction is open.
      Use [with_transaction] when possible. *)
  val commit : t -> unit Io.t

  (** Rollback a transaction. Throws an exception if no transaction is open.
      Use [with_transaction] when possible. *)
  val rollback : t -> unit Io.t

  (** [with_transaction db ?isolation ?access ?deferrable f] wraps your
      function [f] inside a transactional block.
      See [begin_work] for a description of [isolation], [access], and
      [deferrable].
      If [f] throws an exception, the transaction will be rolled back. Otherwise
      the transaction will be commited. It is an error to call [commit] or
      [rollback] manually inside of this function. *)
  val with_transaction
    :  ?isolation:Isolation.t
    -> ?access:Access.t
    -> ?deferrable:bool
    -> t
    -> (t -> 'b Io.t)
    -> 'b Io.t

  module Prepared : sig
    type s [@@deriving sexp_of]

    (** [prepare ?name ?types conn ~query] prepares the statement [query] and
        sets the parameter types to [types].
        If no [name] is given, a random name will be generated.
        If no types are given, then the PostgreSQL engine infers types. *)
    val prepare : ?name:string -> ?types:oid list -> t -> query:string -> s Io.t

    (** [close_statement t] closes a prepared statement and frees
        up any resources. *)
    val close : s -> unit Io.t

    (** [prepare] a query, execute [f], and then [close_statement] *)
    val with_prepare
      :  ?name:string
      -> ?types:oid list
      -> t
      -> query:string
      -> f:(s -> 'a Io.t)
      -> 'a Io.t

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
    val execute : ?portal:string -> s -> params:param list -> row list Io.t

    (** [execute_unit ?portal s ?params] same as execute, but intended
        for database calls that have side-affects rather than returning results *)
    val execute_unit : ?portal:string -> s -> params:param list -> unit Io.t

    val execute_fold
      :  ?portal:string
      -> s
      -> params:param list
      -> init:'accum
      -> f:('accum -> row -> 'accum Io.t)
      -> 'accum Io.t

    val execute_iter
      :  ?portal:string
      -> s
      -> params:param list
      -> f:(row -> unit Io.t)
      -> unit Io.t

    val execute_map
      :  ?portal:string
      -> s
      -> params:param list
      -> f:(row -> 'a Io.t)
      -> 'a list Io.t

    val execute_many : s -> params:param list list -> row list list Io.t

    (** [describe_statement t] describes the statement's parameter types and
        result types. *)
    val describe : s -> (params_description * Result_desc.t list option) Io.t

    (** [close_portal conn ?portal ()] closes a portal and frees up any
        resources. *)
    val close_portal : ?portal:string -> s -> unit Io.t

    (** [describe_portal conn ?portal ()] describes the named or unnamed
          portal's result types. *)
    val describe_portal : ?portal:string -> s -> Result_desc.t list option Io.t
  end

  (** [execute conn ?params query] prepares and executes the statement
      [query] and returns the result. *)
  val execute : ?params:row -> t -> string -> row list Io.t

  (** [execute_unit conn ?params query ] same as execute, but intended
      for database calls that have side-affects rather than returning results *)
  val execute_unit : ?params:row -> t -> string -> unit Io.t

  val execute_fold
    :  ?params:param list
    -> t
    -> string
    -> init:'accum
    -> f:('accum -> row -> 'accum Io.t)
    -> 'accum Io.t

  val execute_map
    :  ?params:param list
    -> t
    -> string
    -> f:(row -> 'a Io.t)
    -> 'a list Io.t

  val execute_iter
    :  ?params:param list
    -> t
    -> string
    -> f:(row -> unit Io.t)
    -> unit Io.t

  (** Prepares a query as in [execute] and then executes it once per set of
      parameters in [params]. This is more efficient than calling [execute]
      in a loop because the query is only prepared once. *)
  val execute_many : t -> query:string -> params:param list list -> row list list Io.t

  (** [simple_query conn query] executes the command(s) in the given [query]
      and returns a list of query results (i.e. if you run two queries, you
      will get a list with two elements: the results of the first query
      followed by the results of the second query. *)
  val simple_query : t -> string -> row list list Io.t
end
