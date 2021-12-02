(lang dune 2.7)
(name obuilder)
(formatting disabled)
(generate_opam_files true)
(source (github ocurrent/obuilder))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(documentation "https://ocurrent.github.io/obuilder/")
(package
 (name obuilder)
 (synopsis "Run build scripts for CI")
 (description
  "OBuilder takes a build script (similar to a Dockerfile) and performs the steps in it in a sandboxed environment.")
 (depends
  (lwt (>= 5.5.0))
  astring
  (fmt (>= 0.8.9))
  logs
  cmdliner
  (tar-unix (>= 2.0.0))
  yojson
  sexplib
  ppx_deriving
  ppx_sexp_conv
  sha
  sqlite3
  (obuilder-spec (= :version))
  (ocaml (>= 4.10.0))
  (alcotest-lwt :with-test)))
(package
 (name obuilder-spec)
 (synopsis "Build specification format")
 (description
  "A library for constructing, reading and writing OBuilder build specification files.")
 (depends
  (fmt (>= 0.8.9))
  sexplib
  astring
  ppx_deriving
  ppx_sexp_conv
  (ocaml (>= 4.10.0))))
