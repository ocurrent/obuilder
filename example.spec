; This script builds OBuilder itself using a snapshot of the ocurrent/opam:debian-10-4.11 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   dune exec -- obuilder build --store=zfs:tank -f example.spec .
;
; The result can then be found in /tank/HASH/rootfs/ (where HASH is displayed at the end of the build).

((from ocurrent/opam@sha256:27504372f75c847ac82eecc4f21599ba81647d377f844bde25325d6852a65760)
 (workdir /src)
 (user (uid 1000) (gid 1000))                           ; Build as the "opam" user
 (run (shell "sudo chown opam /src"))
 (env OPAM_HASH "3332c004db65ef784f67efdadc50982f000b718f") ; Fix the version of opam-repository we want
 (run (shell
   "cd ~/opam-repository \
    && (git cat-file -e $OPAM_HASH || git fetch origin master) \
    && git reset -q --hard $OPAM_HASH \
    && git log --no-decorate -n1 --oneline \
    && opam update -u"))
 (copy (src obuilder.opam) (dst .))                     ; Copy just the opam file first (helps caching)
 (run (shell "opam pin add -yn obuilder.dev ."))
 ; Install OS package dependencies
 (run
  (cache (opam-archives (target /home/opam/.opam/download-cache)))
  (shell "opam depext -y obuilder"))
 ; Install OCaml dependencies
 (run
  (cache (opam-archives (target /home/opam/.opam/download-cache)))
  (shell "opam install --deps-only -t obuilder"))
 (copy                                                  ; Copy the rest of the source code
  (src .)
  (dst /src/)
  (exclude .git _build))
 (run (shell "opam exec -- dune build @install @runtest && rm -rf _build")))    ; Build and test
