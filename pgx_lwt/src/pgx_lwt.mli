module Io_intf = Io_intf

module type S = Pgx.S with type 'a Io.t = 'a Lwt.t

module Make (Io : Io_intf.S) : S
