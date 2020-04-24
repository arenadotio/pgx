(** Helper functions since we don't want a dependency on Core or Batteries. *)

module String : sig
  include module type of String

  val starts_with : string -> string -> bool
  val join : string -> string list -> string
  val implode : char list -> string
  val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  val init : int -> (int -> char) -> string
end

module Option : sig
  val equal : ?cmp:('a -> 'a -> bool) -> 'a option -> 'a option -> bool
  val map : ('a -> 'b) -> 'a option -> 'b option
  val bind : ('a -> 'b option) -> 'a option -> 'b option
end

module List : sig
  include module type of List

  val iteri : (int -> 'a -> unit) -> 'a list -> unit

  (** Like the built-in [List.map], but tail-recursive *)
  val map : ('a -> 'b) -> 'a list -> 'b list
end

(** Necessary for ppx_assert *)
val compare_string : string -> string -> int

val compare_bool : bool -> bool -> int
