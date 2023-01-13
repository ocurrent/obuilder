; This script builds OBuilder itself using a snapshot of the
; ocaml/opam:windows-mingw-ocaml-4.14 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   root=../var
;   dune exec -- obuilder build --docker-backend="$root" -f example.windows.spec .
;
; The result can then be found in the Docker image "obuilder-ROOTID-image-HASH"
; (where HASH is displayed at the end of the build).
; The logs can be found in "$root/logs/HASH.log".
; ROOTID is computed as follows: $(realpath "$(root)" | sha256sum | cut -b -7)

((build dev
	((from ocaml/opam@sha256:1494ed5d4445e10f68cbcaf40aee60d507fbe1726a2d7fd5ba394003b7031b20)
	 (workdir /src)
	 (env OPAM_HASH "696c4b27488b4b0d3ec3929dbe65565cb91764a1") ; Fix the version of opam-repository-mingw we want
	 (shell /cygwin64/bin/bash.exe --login -c)
	 (run
	  (network "nat")
	  (shell
	   "cd /home/opam/opam-repository \
	    && (git cat-file -e $OPAM_HASH || git fetch origin opam2) \
	    && git reset -q --hard $OPAM_HASH \
	    && git --no-pager log --no-decorate -n1 --oneline \
	    && rsync -ar --update --exclude='.git' ./ /cygdrive/c/opam/.opam/repo/default \
	    && ocaml-env exec --64 -- opam update -u"))
         ; opam update -u fails because of patch, so I'm overriding the repo with rsync
	 (shell cmd /S /C)
	 ; Copy just the opam file first (helps caching)
	 (copy (src obuilder-spec.opam obuilder.opam) (dst ./))
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam pin add -yn ."))
	 ; Install OS package dependencies
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam depext -yu obuilder"))
	 ; Install OCaml dependencies
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam install --deps-only -t obuilder-spec"))
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam install --deps-only -t obuilder"))
	 (copy                                                  ; Copy the rest of the source code
	  (src .)
	  (dst /src/)
	  (exclude .git _build _opam duniverse))
	 (run (shell "ocaml-env exec --64 -- dune build @install"))))    ; Build
 ; Now generate a small runtime image with just the resulting binary:
 (from mcr.microsoft.com/windows/servercore:ltsc2022)
 (run (shell "mkdir C:\obuilder"))
 (copy (from (build dev))
       (src /cygwin64/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libsqlite3-0.dll)
       (dst /obuilder/libsqlite3-0.dll))
 (copy (from (build dev))
       (src /src/_build/default/main.exe)
       (dst /obuilder/obuilder.exe))
 (run (shell "/obuilder/obuilder.exe --help")))
