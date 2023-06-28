(lang dune 3.7)
(name obuilder)

(formatting disabled)
(generate_opam_files true)
(source (github ocurrent/obuilder))
(authors
 "Antonin Décimo <antonin@tarides.com>"
 "Arthur Wendling <art.wendling@gmail.com>"
 "David Allsopp <david.allsopp@metastack.com>"
 "Kate <kit.ty.kate@disroot.org>"
 "Lucas Pluvinage <lucas@tarides.com>"
 "Mark Elvers <mark.elvers@tunbury.org>"
 "Patrick Ferris <pf341@patricoferris.com>"
 "Thomas Gazagnaire <thomas@gazagnaire.org>"
 "Thomas Leonard <talex5@gmail.com>"
 "Tim McGilchrist <timmcgil@gmail.com>")
(maintainers "Tim McGilchrist <timmcgil@gmail.com>" "Antonin Décimo <antonin@tarides.com>")
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
  (cmdliner (>= 1.2.0))
  (tar-unix (>= 2.4.0))
  (yojson (>= "1.6.0"))
  sexplib
  ppx_deriving
  ppx_sexp_conv
  (sha (>= 1.15.4))
  sqlite3
  (crunch (and (>= 3.3.1) :build))
  (obuilder-spec (= :version))
  fpath
  (extunix (>= 0.4.0))
  (ocaml (>= 4.14.1))
  (md2mld (>= 0.7.0))
  (alcotest-lwt (and (>= 1.7.0) :with-test))))

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
  (ocaml (>= 4.14.1))))
