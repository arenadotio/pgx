(* A basic example of Pgx_async usage *)
open Core
open Async

let print_table dbh = 
  let query_all = "SELECT * FROM Company" in
  Pgx_async.simple_query dbh query_all
  >>| fun x -> printf !"%{sexp: Pgx.row list list}\n" x

let main () =

  Pgx_async_test.with_temp_db 
    (fun dbh ~db_name:_ -> 
       (** Create the table *)
       print_endline "creating table";
       Pgx_async.simple_query dbh 
         "CREATE TABLE Company( \
          id INT PRIMARY KEY     NOT NULL, \ 
          name           TEXT    NOT NULL, \
          age            INT     NOT NULL, \
          address        VARCHAR(50), \
          salary         REAL)"

       (** Inserting one row with prepare and execute_prepared *)
       >>= fun _ -> 
       print_endline "inserting 1 row";
       let insert_query = "INSERT INTO Company (id, name, age, address, salary) \
                           VALUES ($1, $2, $3, $4, $5)" in
       let params = [ Some "2" ; Some "John" ; Some "21" ; Some "Pittsburgh" ; Some "100000.0" ] in
       Pgx_async.Prepared.(with_prepare dbh ~query:insert_query
                             ~f:(execute ~params))

       (** Inserting multiple rows with prepare_execute_prepared_many *)
       >>= fun _ ->
       print_endline "inserting 2 rows at the same time";
       let params = [[ Some "4" ; Some "Robert" ; Some "21" ; Some "Pittsburgh" ; Some "100000.0"]
                    ;[ Some "3" ; Some "John"   ; Some "23" ; Some "New York"   ; Some "30000.0" ]] in
       Pgx_async.execute_many dbh ~query:insert_query ~params

       (** Select a single row, with id = 3*)
       >>= fun _ -> 
       print_endline "selecting name with id = 3";
       let query_select_id = "SELECT name FROM Company WHERE id = $1" in
       let params = [ Some "3" ] in
       Pgx_async.execute dbh query_select_id ~params
       >>= fun rows -> 
       printf !"%{sexp: Pgx.row list}\n" rows;

       (** Alter table by deleting the column address *)
       print_endline "altering table by dropping column address";
       let query = "ALTER TABLE Company
                     DROP COLUMN address" in
       Pgx_async.simple_query dbh query 

       >>= fun _ -> print_table dbh 

       (** adding a new column gender *)
       >>= fun _ -> 
       print_endline "altering table by adding gender column";
       let query = "ALTER TABLE Company
                     ADD COLUMN gender TEXT" in
       Pgx_async.simple_query dbh query 

       >>= fun _ -> print_table dbh 

       >>= fun _ -> 
       print_endline "updating John's gender";
       let query = "UPDATE Company 
                     SET gender = $1
                     WHERE name = $2" in
       let params = [ Some "male" ; Some "John" ] in
       Pgx_async.execute dbh query ~params

       >>= fun _ -> print_table dbh 

       (** Update a row *)
       >>= fun _ ->
       print_endline "updating person with id = 4";
       let query = "UPDATE Company 
                     SET name = $1, age = $2
                     WHERE id = $3" in
       let params = [ Some "Roger" ; Some "34" ; Some "4" ] in
       Pgx_async.execute dbh query ~params 

       >>= fun _ -> print_table dbh 

       (** Delete a row *)
       >>= fun _ ->
       print_endline "deleting person with id = 4";
       let query = "DELETE FROM Company 
                     WHERE id= $1" in
       let params = [ Some "4" ] in
       Pgx_async.execute dbh query ~params

       >>= fun _ -> print_table dbh 

       (** Begin Transaction using Pgx_async.transact *)
       >>= fun () -> 
       print_endline "start transaction";
       Monitor.try_with  (fun () -> 
         Pgx_async.with_transaction dbh 
           (fun dbh ->
              (** Deleting id = 2 *)
              print_endline "deleting person with id = 2";
              let query = "DELETE FROM Company WHERE id = $1" in
              let params = [ Some "2" ] in
              Pgx_async.execute dbh query ~params

              >>= fun _ -> print_table dbh 

              (** if an exception is thrown, rollback will be done *)
              >>= fun () -> failwith "example exception"
           )
         >>| function
         | Ok v -> v 
         | Error _e -> print_endline "caught exception"
       )

       >>= fun _ -> 
       print_endline "exception was thrown in transaction, so rollback executed";
       print_table dbh

       (** Begin Transaction using manual transaction *)
       >>= fun () -> Pgx_async.begin_work dbh
       >>= fun tx -> 
       print_endline "start transaction";

       (** Add a guy with id = 5 *)
       print_endline "inserting person with id = 5";
       let query = "INSERT INTO Company (id, name, age, salary) VALUES ($1, $2, $3, $4)" in
       let params = [ Some "5" ; Some "Alex" ; Some "15" ; Some "60000.0" ] in
       Pgx_async.execute dbh query ~params

       >>= fun _ -> 
       print_endline "commit to inserting person with id = 5";

       Pgx_async.commit tx

       (** rollback and commit both automatically exit the transaction *)
       >>= fun _ -> print_table dbh 

       (** begin work again *)
       >>= fun () -> Pgx_async.begin_work dbh
       >>= fun _ -> 
       print_endline "start transaction";

       (** update alex's salary by increasing it *)
       print_endline "update alex's salary";
       let query = "UPDATE Company 
                     SET salary = $1
                     WHERE name = $2" in
       let params = [ Some "70000.0" ; Some "Alex" ] in
       Pgx_async.execute dbh query ~params 

       >>= fun _ -> print_table dbh 

       (** rollback to alex's original salary *)
       >>= fun _ -> 
       print_endline "rollback to alex's original salary";
       Pgx_async.rollback dbh

       >>= fun _ -> print_table dbh

       (** select multiple rows using execute_many *)
       >>= fun _ -> 
       print_endline "selecting multiple lines at same time using execute_many. Make sure order is correct!";
       let params = [[Some "John"; Some "50000"];[Some "Alex"; Some "0"]] in
       Pgx_async.execute_many dbh ~query:"SELECT * FROM Company WHERE name = $1 and salary >= $2" ~params
       >>| fun x -> printf !"%{sexp: Pgx.row list list}\n" x
    )

let () =
  let summary = "example command" in
  Command.async_spec ~summary Command.Spec.empty main
  |> Command.run
