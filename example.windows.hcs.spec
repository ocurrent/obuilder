; This script builds OBuilder itself using the HCS backend on Windows.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   obuilder build -f example.windows.hcs.spec . --store=hcs:C:\obuilder
;

((from ocaml/opam:windows-server-msvc-ltsc2025-ocaml-5.4)
 (workdir "C:/src")
 ; Copy just the opam files first (helps caching)
 (copy (src obuilder-spec.opam obuilder.opam) (dst ./))
 ; Create a dummy dune-project so dune subst works for pinned dev packages
 (run (shell "echo (lang dune 3.0)> dune-project"))
 (run (shell "opam pin add -yn ."))
 ; Install OCaml dependencies
 (run
  (network host)
  (shell "opam install --deps-only -t obuilder"))
 ; Copy the rest of the source code
 (copy
  (src .)
  (dst "C:/src/")
  (exclude .git _build _opam))
 ; Build and test
 (run (shell "opam exec -- dune build @install @runtest")))
