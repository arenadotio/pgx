## 2.1 (unreleased)
### Breaking changes

* pgx_lwt_mirage now requires conduit 2.3 instead of 2.2 (https://github.com/arenadotio/pgx/pull/117).

## 2.0 (2021-05-12)

### Breaking changes

* The Pgx module is now wrapped, which means `Pgx_aux`, `Types`, `Access`, etc. aren't added to the global scope.
  The main result of this is that `Pgx_value` now needs to be accessed as `Pgx.Value`.
  (https://github.com/arenadotio/pgx/pull/103)
* `Pgx_async.connect` and `with_conn` now have an additional optional `?ssl` argument (see below).

### Added

* Pgx_async now supports TLS connections using Conduit_async. This is enabled by default and can be controlled with the
  new `?ssl` argument to `connect` and `with_conn`.
  (https://github.com/arenadotio/pgx/pull/108)

### Fixed

* Improved message for authentication errors. Previously these raised `Pgx_eof`, and now they raise
  `PostgreSQL_Error("Failed to authenticate with postgres server", additional details ...)`.
  (https://github.com/arenadotio/pgx/pull/105)

### Changed

* Support new Mirage-conduit timeout argument (https://github.com/arenadotio/pgx/pull/95).

## 1.0 (2020-05-08)

### Breaking changes

* Pgx_value.t is an opaque type now. Use `Pgx_value.of/to` converters. Note that these converters are _not_ equivalent
  to the OCaml functions like `bool_of_string` or `float_of_string`, and that for bytea data, you need to use
  `Pgx_value.of/to_binary`, not `Pgx_value.of/to_string`.
* Pgx_lwt has been renamed Pgx_lwt_unix.
* `Pgx.execute` now uses the unnamed prepare statement. In most cases this should not affect anything, but if you were
  relying on Pgx not internally using the unnamed prepared statement, you will need to fix your code. If you run into
  this, the fix is to use `Pgx.with_prepared` and name your prepared statement.
* `Pgx_value.of_inet`/`to_inet` now use `Ipaddr.t` from the `ipaddr` library instead of `Unix.inet_addr`.

### Added

* `Pgx_value.of_binary` and `Pgx_value.to_binary` added for bytea data.
* Add `execute_map` helper to Pgx
* Add `execute_pipe` helper to Pgx_async
* Add `execute_unit` helper to Pgx
* Break out `Pgx_value_core` library, which will allow users of Pgx_unix and Pgx_lwt to use the `Core_kernel.Tim` and
  `Date` types. This is still included by default in Pgx_async.
* Added Pgx_lwt_mirage
* Pgx_value types now all implement `compare` and `sexp_of`

### Fixed

* Pgx no longer assumes all strings are binary data. Strings must be valid varchar data in the database's encoding.
  Use `Pgx_value.of/to_binary` with bytea columns if you want binary.
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
