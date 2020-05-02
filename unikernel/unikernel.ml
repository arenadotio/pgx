open Lwt.Syntax

module Make
    (RANDOM : Mirage_random.S)
    (PCLOCK : Mirage_clock.PCLOCK)
    (MCLOCK : Mirage_clock.MCLOCK)
    (STACK : Mirage_stack.V4) =
struct
  module Pgx_mirage = Pgx_lwt_mirage.Make (RANDOM) (MCLOCK) (STACK)
  module Logs_reporter = Mirage_logs.Make (PCLOCK)

  type user =
    { id : int
    ; email : string
    }

  let get_users ~port ~user ~host ~password ~database pgx () =
    Logs.info (fun m -> m "Fetching users");
    let module P = (val pgx : Pgx_lwt.S.Pgx_lwt) in
    P.with_conn ~user ~host ~password ~port ~database (fun conn ->
        let+ rows = P.execute conn "SELECT * FROM USERS" in
        List.map
          (fun row ->
            match row with
            | [ id; email ] ->
              { id = Pgx.Value.to_int_exn id; email = Pgx.Value.to_string_exn email }
            | _ -> failwith "invalid data")
          rows)
  ;;

  let print_users users =
    let+ users = users in
    List.iter
      (fun { id; email } -> Logs.info (fun m -> m "{id = %d; email = %s}\n" id email))
      users
  ;;

  let start _random _pclock _mclock stack =
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run)
    @@ fun () ->
    let port = Key_gen.pgport () in
    let host = Key_gen.pghost () in
    let user = Key_gen.pguser () in
    let password = Key_gen.pgpassword () in
    print_users
      (get_users
         ~port
         ~host
         ~user
         ~password
         ~database:"playground"
         (Pgx_mirage.create stack)
         ())
  ;;
end
