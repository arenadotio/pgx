### 0.1 (2018-05-31)

Initial release since fork from PG'OCaml.

 * More tests
 * More consistent use of async API's
 * Addition of Pgx.Value for hopefully easier conversion to and
   from DB types
 * Safe handling of concurrent queries (not any faster, but they
   won't crash)
 * Improved interface for prepared statements to make it harder
   to execute non-existent ones
