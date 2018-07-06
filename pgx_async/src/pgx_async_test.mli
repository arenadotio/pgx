(** Testing library for code that uses postgres *)

open Async

val set_to_default_db : unit -> unit

type 'a new_db_callback = Pgx_async.t -> db_name:string -> 'a Deferred.t

val with_temp_db : 'a new_db_callback -> 'a Deferred.t
(** [with_temp_db f] creates a temporary database and executes [f] with a database
    handle to this db and the name of the db. Once [f] executes or raises, the temp database
    will be deleted. *)
