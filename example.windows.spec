; This script builds OBuilder itself using a snapshot of the
; ocaml/opam:windows-mingw-ltsc2022-ocaml-4.14 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   root=../var
;   dune exec -- obuilder build --store="docker:$root" -f example.windows.spec .
;
; The result can then be found in the Docker image "obuilder-ROOTID-image-HASH"
; (where HASH is displayed at the end of the build).
; The logs can be found in "$root/logs/HASH.log".
; ROOTID is computed as follows: $(realpath "$(root)" | sha256sum | cut -b -7)

((build dev
	((from ocaml/opam@sha256:487f82160de4275a7219e3e1d676c5a9afddd110d1b146909397be62e8e11bc7)
	 (workdir /src)
	 (env OPAM_REPO_MINGW_HASH "921b0eceb594f96c0c7f40bb2676783be4362aeb") ; Fix the version of opam-repository-mingw we want
	 (env OPAM_HASH "15b381eeae1aa1c8b67b214ce1739344717aae89") ; Fix the version of opam-repository we want
	 (shell /cygwin64/bin/bash.exe --login -c)
	 (run
	  (network "nat")
	  (shell
	   "cd /home/opam && mv opam-repository opam-repository-mingw \
	    && opam repository set-url default file://C:/cygwin64/home/opam/opam-repository-mingw \
	    && git clone https://github.com/ocaml/opam-repository.git \
	    && opam repository add --all --rank=2 opam file://C:/cygwin64/home/opam/opam-repository \
	    && git config --global --add safe.directory /home/opam/opam-repository-mingw"))
	 (run
	  (network "nat")
	  (shell
	   "cd /home/opam/opam-repository-mingw \
	    && (git cat-file -e $OPAM_REPO_MINGW_HASH || git fetch origin sunset) \
	    && git reset -q --hard $OPAM_REPO_MINGW_HASH \
	    && git --no-pager log --no-decorate -n1 --oneline \
	    && rsync -ar --update --exclude='.git' ./ /cygdrive/c/opam/.opam/repo/default"))
	 ; opam update -u fails because of patch, so I'm overriding the repo with rsync
	 (run
	  (network "nat")
	  (shell
	   "cd /home/opam/opam-repository \
	    && (git cat-file -e $OPAM_HASH || git fetch origin master) \
	    && git reset -q --hard $OPAM_HASH \
	    && git --no-pager log --no-decorate -n1 --oneline \
	    && rsync -ar --update --exclude='.git' ./ /cygdrive/c/opam/.opam/repo/opam"))
	 ; opam update -u fails because of patch, so I'm overriding the repo with rsync
	 (run
	  (network "nat")
	  (shell "ocaml-env exec --64 -- opam update -u"))
	 (shell cmd /S /C)
	 ; Copy just the opam file first (helps caching)
	 (copy (src obuilder-spec-opam.opam obuilder-spec.opam obuilder.opam) (dst ./))
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam pin add -yn ."))
	 ; Install OS package dependencies
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam depext -yu obuilder obuilder-spec obuilder-spec-opam"))
	 ; Install OCaml dependencies
	 (run
	  (network "nat")
	  (cache (opam-archives (target /opam/.opam/download-cache)))
	  (shell "ocaml-env exec --64 -- opam install --deps-only -t ."))
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
