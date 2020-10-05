(lang dune 2.7)

(name obuilder)
(formatting disabled)

(generate_opam_files true)

(source (github ocurrent/obuilder))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name obuilder)
 (synopsis "Run build scripts for CI")
 (depends
  lwt
  astring
  fmt
  logs
  cmdliner
  tar-unix
  yojson
  sexplib
  ppx_deriving
  ppx_sexp_conv
  sha
  sqlite3
  dockerfile
  obuilder-spec
  (ocaml (>= 4.10.0))
  (alcotest-lwt :with-test)
))

(package
 (name obuilder-spec)
 (synopsis "Build specification format")
 (depends
  sexplib
  astring
  ppx_deriving
  ppx_sexp_conv
  dockerfile
  (ocaml (>= 4.10.0))
))
