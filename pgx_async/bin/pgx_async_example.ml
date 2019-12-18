(* A basic example of Pgx_async usage *)
open Core
open Async

module Employee = struct
  let create db =
    Pgx_async.simple_query db {|
      CREATE TEMPORARY TABLE Employee (
        id SERIAL PRIMARY KEY,
        name VARCHAR(100) NOT NULL UNIQUE);
    |}
    |> Deferred.ignore_m

  (* This function lets us insert multiple users relatively efficiently *)
  let insert_many db names =
    let params =
      List.map names ~f:(fun name ->
        Pgx_async.Value.[ of_string name ])
    in
    Pgx_async.execute_many db ~params ~query:{|
      INSERT INTO Employee (name)
      VALUES ($1)
      RETURNING id
    |}
    >>| List.map ~f:(function
      | [[ id ]] -> Pgx.Value.to_int_exn id
      | _ -> assert false)

  let insert ~name db =
    insert_many db [ name ]
    >>| List.hd_exn
end

module Facility = struct
  let create db =
    Pgx_async.simple_query db {|
      CREATE TEMPORARY TABLE Facility (
        id SERIAL PRIMARY KEY,
        name VARCHAR(100) NOT NULL UNIQUE,
        director_id INT REFERENCES Employee(id) ON DELETE SET NULL);

      CREATE INDEX facility_director_id ON Facility (director_id);
    |}
    |> Deferred.ignore_m

  let insert ~name ?director_id db =
    let params = Pgx_async.Value.[ of_string name ; opt of_int director_id ] in
    Pgx_async.execute db ~params {|
      INSERT INTO Facility (name, director_id)
      VALUES ($1, $2)
      RETURNING id
    |}
    >>| function
    | [[ id ]] -> Pgx.Value.to_int_exn id
    | _ -> assert false

  let all_name_and_director_name db =
    Pgx_async.execute db {|
      SELECT f.name, e.name
      FROM Facility f
      LEFT JOIN Employee e ON e.id = f.director_id
    |}
    >>| List.map ~f:(function
      | [ name ; director_name ] ->
        Pgx.Value.(to_string_exn name, to_string director_name)
      | _ -> assert false)

  let reassign_director db ~director_id ~from_facility_id ~to_facility_id =
    (* Note: with_transaction doesn't currently have any special handling
       for concurrent queries *)
    Pgx_async.with_transaction db @@ fun db ->
    let params = Pgx.Value.[ of_int director_id ; of_int from_facility_id ] in
    Pgx_async.execute db ~params {|
      UPDATE Facility SET director_id = NULL WHERE id = $2 AND director_id = $1
    |}
    >>= fun _ ->
    let params = Pgx.Value.[ of_int director_id ; of_int to_facility_id ] in
    Pgx_async.execute db ~params {|
      UPDATE Facility SET director_id = $1 WHERE id = $2
    |}
    |> Deferred.ignore_m
end

let setup db =
  Employee.create db
  >>= fun () ->
  Facility.create db

let main () =
  Pgx_async.with_conn @@ fun db ->
  setup db
  >>= fun () ->
  let%bind steve_id = Employee.insert ~name:"Steve" db in
  (* Parallel queries are not an error, but will execute in serial *)
  [ Facility.insert ~name:"Headquarters" ~director_id:steve_id db
  ; Facility.insert ~name:"New Office" db ]
  |> Deferred.all
  >>= function
  | [ headquarters_id ; new_office_id ] ->
    Facility.all_name_and_director_name db
    >>| List.iter ~f:(fun (name, director_name) ->
      let director_name = Option.value director_name ~default:"(none)" in
      printf "The director of %s is %s\n" name director_name)
    >>= fun () ->
    print_endline "Re-assigning Steve to the New Office";
    Facility.reassign_director db ~director_id:steve_id ~from_facility_id:headquarters_id ~to_facility_id:new_office_id
    >>= fun () ->
    Facility.all_name_and_director_name db
    >>| List.iter ~f:(fun (name, director_name) ->
      let director_name = Option.value director_name ~default:"(none)" in
      printf "The director of %s is %s\n" name director_name)
  | _ -> assert false

let () =
  let summary = "Pgx_async example" in
  Command.async ~summary (Command.Param.return main)
  |> Command.run
