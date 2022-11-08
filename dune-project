(lang dune 3.3)
(name obuilder)
(formatting disabled)
(generate_opam_files true)
(source (github ocurrent/obuilder))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(license "Apache-2.0")
(documentation "https://ocurrent.github.io/obuilder/")
(package
 (name obuilder)
 (synopsis "Run build scripts for CI")
 (description
  "OBuilder takes a build script (similar to a Dockerfile) and performs the steps in it in a sandboxed environment.")
 (depends
  (lwt (>= 5.6.1))
  astring
  (fmt (>= 0.8.9))
  logs
  (cmdliner (>= 1.1.0))
  (tar-unix (>= 2.0.1))
  (yojson (>= "1.6.0"))
  sexplib
  ppx_deriving
  ppx_sexp_conv
  (sha (>= 1.15.2))
  sqlite3
  (obuilder-spec (= :version))
  (ocaml (>= 4.14.0))
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
