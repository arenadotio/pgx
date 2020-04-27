open Mirage

let main = foreign ~packages:[ package "pgx" ] "Unikernel" job
let () = register "hello" [ main ]
