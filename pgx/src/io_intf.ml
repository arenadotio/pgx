(** The interface implemented by IO backends (Async, Lwt, Unix, etc.) *)
module type S = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  type in_channel
  val sexp_of_in_channel : in_channel -> Sexplib0.Sexp.t
  type out_channel
  val sexp_of_out_channel : out_channel -> Sexplib0.Sexp.t
  type sockaddr =
    | Unix of string
    | Inet of string * int
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

  module Sequencer : sig
    type 'a monad = 'a t
    type 'a t
    val sexp_of_t : 'a t -> Sexplib0.Sexp.t
    val create : 'a -> 'a t
    val enqueue : 'a t -> ('a -> 'b monad) -> 'b monad
  end
end
