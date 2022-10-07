; This script builds OBuilder itself using a snapshot of the ocaml/opam:debian-11-ocaml-4.14 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   dune exec -- obuilder build --store=zfs:tank -f example.spec .
;
; The result can then be found in /tank/HASH/rootfs/ (where HASH is displayed at the end of the build).

((build dev
	((from ocaml/opam@sha256:5b9de826b22c77a0654519d0959536f93a6ffd7020712a8b1c3437445e031e04)
	 (workdir /src)
	 (user (uid 1000) (gid 1000))                           ; Build as the "opam" user
	 (run (shell "sudo chown opam /src"))
	 (env OPAM_HASH "97da9a1b68b824a65a09e5f7d071fcf2da35bd1b") ; Fix the version of opam-repository we want
	 (run
	  (network host)
	  (shell "sudo apt-get --allow-releaseinfo-change update"))
	 (run
	  (network host)
	  (shell
	   "cd ~/opam-repository \
	    && (git cat-file -e $OPAM_HASH || git fetch origin master) \
	    && git reset -q --hard $OPAM_HASH \
	    && git --no-pager log --no-decorate -n1 --oneline \
	    && opam update -u"))
	 ; Copy just the opam file first (helps caching)
	 (copy (src obuilder-spec.opam obuilder.opam) (dst ./))
	 (run (shell "opam pin add -yn ."))
	 ; Install OS package dependencies
	 (run
	  (network host)
	  (cache (opam-archives (target /home/opam/.opam/download-cache)))
	  (shell "opam depext -yu obuilder"))
	 ; Install OCaml dependencies
	 (run
	  (network host)
	  (cache (opam-archives (target /home/opam/.opam/download-cache)))
	  (shell "opam install --deps-only -t obuilder"))
	 (copy                                                  ; Copy the rest of the source code
	  (src .)
	  (dst /src/)
	  (exclude .git _build _opam))
	 (run (shell "opam exec -- dune build @install @runtest"))))    ; Build and test
 ; Now generate a small runtime image with just the resulting binary:
 (from debian:11)
 (run
  (network host)
  (shell "apt-get update && apt-get install -y libsqlite3-0 --no-install-recommends"))
 (copy (from (build dev))
       (src /src/_build/default/main.exe)
       (dst /usr/local/bin/obuilder))
 (run (shell "obuilder --help")))
