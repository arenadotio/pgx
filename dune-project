(lang dune 3.2)

(name pgx)

(generate_opam_files true)

(license "LGPL-2.0-only with OCaml-LGPL-linking-exception")

(maintainers "Arena Developers <silver-snakes@arena.io>")

(authors "Arena Developers <silver-snakes@arena.io>")

(source
 (github arenadotio/pgx))

(documentation "https://arenadotio.github.io/pgx")

(package
 (name pgx)
 (synopsis "Pure-OCaml PostgreSQL client library")
 (description
  "PGX is a pure-OCaml PostgreSQL client library, supporting Async, LWT, or synchronous operations.")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.0.0)))
  (bisect_ppx
   (and
    :dev
    (>= 2.0.0)))
  (dune
   (>= 3.2))
  hex
  ipaddr
  camlp-streams
  (ocaml
   (>= 4.08))
  (odoc :with-doc)
  (ppx_compare
   (>= v0.13.0))
  (ppx_custom_printf
   (>= v0.13.0))
  (ppx_sexp_conv
   (>= v0.13.0))
  (re
   (>= 1.5.0))
  (sexplib0
   (>= v0.13.0))
  uuidm))

(package
 (name pgx_unix)
 (synopsis
  "PGX using the standard library's Unix module for IO (synchronous)")
 (description
  "PGX using the standard library's Unix module for IO (synchronous)")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.0.0)))
  (base64
   (and
    :with-test
    (>= 3.0.0)))
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))))

(package
 (name pgx_async)
 (synopsis "Pgx using Async for IO")
 (description "Pgx using Async for IO")
 (depends
  (alcotest-async
   (and
    :with-test
    (>= "1.0.0")))
  (async_kernel
   (>= "v0.13.0"))
  (async_unix
   (>= "v0.13.0"))
  async_ssl
  (base64
   (and
    :with-test
    (>= 3.0.0)))
  (conduit-async
   (>= 1.5.0))
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))
  (pgx_value_core
   (= :version))))

(package
 (name pgx_lwt)
 (synopsis "Pgx using Lwt for IO")
 (description "Pgx using Lwt for IO")
 (depends
  lwt
  logs
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))))

(package
 (name pgx_lwt_unix)
 (synopsis "Pgx using Lwt and Unix libraries for IO")
 (description "Pgx using Lwt and Unix libraries for IO")
 (depends
  (alcotest-lwt
   (and
    :with-test
    (>= "1.0.0")))
  (base64
   (and
    :with-test
    (>= 3.0.0)))
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))
  (pgx_lwt
   (= :version))))

(package
 (name pgx_value_core)
 (synopsis "Pgx_value converters for Core types like Date and Time")
 (description "Pgx_value converters for Core types like Date and Time")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.0.0)))
  (core_kernel
   (>= "v0.13.0"))
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))))

(package
 (name pgx_value_ptime)
 (synopsis "Pgx_value converters for Ptime types")
 (description "Pgx_value converters for Ptime types")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.0.0)))
  (ptime
   (>= 0.8.3))
  (ocaml
   (>= 4.08))
  (pgx
   (= :version))))

(package
 (name pgx_lwt_mirage)
 (synopsis "Pgx using Lwt on Mirage for IO")
 (description "Pgx using Lwt on Mirage for IO")
 (depends
  lwt
  (ocaml
   (>= 4.08))
  logs
  mirage-channel
  (conduit-mirage
    (>= 2.3.0))
  (dns-client
    (>= 6.0.0))
  mirage-random
  mirage-time
  mirage-clock
  (tcpip
    (>= 7.0.0))
  (pgx
   (= :version))
  (pgx_lwt
   (= :version))))
