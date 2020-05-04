module IO_intf = Io_intf

module type S = Pgx.S with type 'a IO.t = 'a Lwt.t

module Make (IO : Io_intf.S) : S
