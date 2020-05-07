## 0.2 (2020-05-07)

### Breaking changes

* Pgx_lwt has been renamed Pgx_lwt_unix.
* `Pgx.execute` now uses the unnamed prepare statement. In most cases this should not affect anything, but if you were
  relying on Pgx not internally using the unnamed prepared statement, you will need to fix your code. If you run into
  this, the fix is to use `Pgx.with_prepared` and name your prepared statement.
* `Pgx_value.of_inet`/`to_inet` now use `Ipaddr.t` from the `ipaddr` library instead of `Unix.inet_addr`.

### Added

* Add `execute_map` helper to Pgx
* Add `execute_pipe` helper to Pgx_async
* Add `execute_unit` helper to Pgx
* Break out `Pgx_value_core` library, which will allow users of Pgx_unix and Pgx_lwt to use the `Core_kernel.Tim` and
  `Date` types. This is still included by default in Pgx_async.
* Added Pgx_lwt_mirage

### Fixed

* Use a tail-recursive `List.map` implementation
* Use `Unix.getuid` + `Lwt_unix.getpwuid` instead of `Lwt.getlogin` for the default username, since `getlogin` fails
  in some cases.
* Use int64 to keep track of prepared statements just in case someone prepares several million statements in one program

### Changed

* Re-raise exceptions with backtraces if possible.
* Pgx_async uses Async.Log for logging instead of printing directly to stderr
* Use Sexplib0 instead of Sexplib
* Use the Query protocol for parameterless `execute` instead of Prepare + Bind
* Use the unnamed prepared statement for `execute`
* Use `ipaddr` library instead of `Unix.inet_addr`
* Split Pgx_lwt into Pgx_lwt_unix and Pgx_lwt_mirage

## 0.1 (2018-05-31)

Initial release since fork from PG'OCaml.

* More tests
* More consistent use of async API's
* Addition of Pgx.Value for hopefully easier conversion to and
  from DB types
* Safe handling of concurrent queries (not any faster, but they
  won't crash)
* Improved interface for prepared statements to make it harder
  to execute non-existent ones
