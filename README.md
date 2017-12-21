[![CircleCI](https://circleci.com/gh/arenadotio/pgx.svg?style=shield)](https://circleci.com/gh/arenadotio/pgx)

PGX is a pure-OCaml PostgreSQL client library, supporting Async, LWT, or
synchronous operations.

**This is an early release. The API is likely to change significantly before
the 1.0 release.**

This library focuses on correctness and safety, with features like:

 - It is nearly impossible to try to execute a prepared statement that hasn't
   been prepared.
 - Trying to run multiple queries at the same time will work properly (although
   there's no performance benefit, since we currently don't send queries in
   parallel).
 - Lots of automated tests.
 - `Pgx.Value` for parameters and returned data, encouraging people to use
   the built-in converters instead of trying to handle everything as a string.
 - Async and LWT support are built in, no need to write your own IO module.

We also provide a relatively high-level interface, like `Pgx_async.execute_pipe`,
which prepares a statement, executes it with the given parameters, returns an
`Async.Pipe.Reader.t` (so you can stream results), and unprepares the statement
when the query is finished.

Significant portions of the code come from [PG'Ocaml](http://pgocaml.forge.ocamlcore.org/).

## Setup

```
opam pin add pgx https://github.com/arenadotio/pgx.git
```

## Examples

See [pgx_async/bin/pgx_async_example.ml](pgx_async/bin/pgx_async_example.ml).
