module S = S
module Make (IO : S.IO) : Pgx.S with type 'a IO.t = 'a Lwt.t
