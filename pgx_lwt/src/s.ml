module type IO = sig
  type in_channel
  type out_channel

  type sockaddr =
    | Unix of string
    | Inet of string * int

  val output_char : out_channel -> char -> unit Lwt.t
  val output_string : out_channel -> string -> unit Lwt.t
  val flush : out_channel -> unit Lwt.t
  val input_char : in_channel -> char Lwt.t
  val really_input : in_channel -> bytes -> int -> int -> unit Lwt.t
  val close_in : in_channel -> unit Lwt.t
  val getlogin : unit -> string Lwt.t
  val open_connection : sockaddr -> (in_channel * out_channel) Lwt.t
end

module type Pgx_impl = sig
  include Pgx.S with type 'a IO.t = 'a Lwt.t
end
