let dockerfile =
  let open Dockerfile in
  {
    from = "ocurrent/opam@sha256:27504372f75c847ac82eecc4f21599ba81647d377f844bde25325d6852a65760";
    ops =
      comment "debian-10-4.11" ::
      workdir "/src" ::
      user ~uid:1000 ~gid:1000 ::
      run "sudo chown opam /src" ::
      run "cd ~/opam-repository && (git cat-file -e 9f2b866093e41bae017292155e8f2cf85889a027 || git fetch origin master) && git reset -q --hard 9f2b866093e41bae017292155e8f2cf85889a027 && git log --no-decorate -n1 --oneline && opam update -u" ::
      run {|mkdir -p "ocurrent/" "ocluster/" "./"|} ::
      copy ["ocurrent/current_web.opam"; "ocurrent/current_slack.opam"; "ocurrent/current_rpc.opam"; "ocurrent/current_incr.opam"; "ocurrent/current_github.opam"; "ocurrent/current_git.opam"; "ocurrent/current_examples.opam"; "ocurrent/current_docker.opam"; "ocurrent/current_ansi.opam"; "ocurrent/current.opam"] "ocurrent/" ::
      copy ["ocluster/ocluster.opam"; "ocluster/ocluster-api.opam"; "ocluster/current_ocluster.opam"] "ocluster/" ::
      copy ["ocaml-ci-web.opam"; "ocaml-ci-solver.opam"; "ocaml-ci-service.opam"; "ocaml-ci-client.opam"; "ocaml-ci-api.opam"] "./" ::
      run {|opam pin add -yn current_web.dev "ocurrent/" && \
         opam pin add -yn current_slack.dev "ocurrent/" && \
         opam pin add -yn current_rpc.dev "ocurrent/" && \
         opam pin add -yn current_incr.dev "ocurrent/" && \
         opam pin add -yn current_github.dev "ocurrent/" && \
         opam pin add -yn current_git.dev "ocurrent/" && \
         opam pin add -yn current_examples.dev "ocurrent/" && \
         opam pin add -yn current_docker.dev "ocurrent/" && \
         opam pin add -yn current_ansi.dev "ocurrent/" && \
         opam pin add -yn current.dev "ocurrent/" && \
         opam pin add -yn ocluster.dev "ocluster/" && \
         opam pin add -yn ocluster-api.dev "ocluster/" && \
         opam pin add -yn current_ocluster.dev "ocluster/" && \
         opam pin add -yn ocaml-ci-web.dev "./" && \
         opam pin add -yn ocaml-ci-solver.dev "./" && \
         opam pin add -yn ocaml-ci-service.dev "./" && \
         opam pin add -yn ocaml-ci-client.dev "./" && \
         opam pin add -yn ocaml-ci-api.dev "./"|} ::
      env "DEPS" "0install-solver.2.17 alcotest.1.2.2 alcotest-lwt.1.2.2 angstrom.0.14.1 asetmap.0.8.1 asn1-combinators.0.2.2 astring.0.8.5 base.v0.14.0 base-bigarray.base base-bytes.base base-threads.base base-unix.base base64.3.4.0 bigarray-compat.1.0.0 bigarray-overlap.0.2.0 bigstringaf.0.6.1 biniou.1.2.1 bos.0.2.0 capnp.3.4.0 capnp-rpc.0.8.0 capnp-rpc-lwt.0.8.0 capnp-rpc-net.0.8.0 capnp-rpc-unix.0.8.0 checkseum.0.2.1 cmdliner.1.0.4 cohttp.2.5.4 cohttp-lwt.2.5.4 cohttp-lwt-unix.2.5.4 conduit.2.1.0 conduit-lwt.2.1.0 conduit-lwt-unix.2.2.2 conf-capnproto.0 conf-gmp.1 conf-gmp-powm-sec.1 conf-libev.4-11 conf-m4.1 conf-perl.1 conf-pkg-config.1.3 conf-sqlite3.1 cppo.1.6.6 cpuid.0.1.2 csexp.1.3.1 cstruct.5.2.0 cstruct-lwt.5.2.0 cstruct-sexp.5.2.0 current.dev current_ansi.dev current_docker.dev current_git.dev current_github.dev current_incr.dev current_rpc.dev current_web.dev decompress.0.9.1 digestif.0.9.0 dockerfile.7.0.0 dockerfile-opam.7.0.0 domain-name.0.3.0 duff.0.2 dune.2.7.1 dune-configurator.2.7.1 duration.0.1.3 easy-format.1.3.2 encore.0.5 eqaf.0.7 fiat-p256.0.2.1 fieldslib.v0.14.0 fmt.0.8.8 fpath.0.7.2 git.2.1.3 git-http.2.1.3 git-unix.2.1.3 gmap.0.3.0 hacl_x25519.0.2.0 hex.1.4.0 hkdf.1.0.4 ipaddr.5.0.0 ipaddr-sexp.5.0.0 irmin-watcher.0.3.0 jbuilder.1.0+beta20.2 jsonm.1.0.1 ke.0.4 logs.0.7.0 lru.0.3.0 lwt.5.3.0 lwt-dllist.1.0.0 macaddr.5.0.0 magic-mime.1.1.2 mirage-clock.3.0.1 mirage-crypto.0.8.5 mirage-crypto-pk.0.8.5 mirage-crypto-rng.0.8.5 mirage-device.2.0.0 mirage-flow.2.0.1 mirage-kv.3.0.1 mirage-no-solo5.1 mirage-no-xen.1 mmap.1.1.0 mtime.1.2.0 nocrypto.0.5.4-2 num.1.3 ocaml.4.11.1 ocaml-base-compiler.4.11.1 ocaml-compiler-libs.v0.12.1 ocaml-config.1 ocaml-migrate-parsetree.1.7.3 ocaml-version.3.0.0 ocamlbuild.0.14.0 ocamlfind.1.8.1 ocamlgraph.1.8.8 ocb-stubblr.0.1.1-1 ocluster-api.dev ocplib-endian.1.1 opam-0install.0.2 opam-core.2.0.7 opam-file-format.2.1.0 opam-format.2.0.7 opam-repository.2.0.7 opam-state.2.0.7 optint.0.0.4 parsexp.v0.14.0 ppx_cstruct.5.2.0 ppx_derivers.1.2.1 ppx_deriving.4.5 ppx_deriving_yojson.3.5.3 ppx_fields_conv.v0.14.1 ppx_sexp_conv.v0.14.1 ppx_tools.6.2 ppx_tools_versioned.5.4.0 ppxfind.1.4 ppxlib.0.15.0 prometheus.0.7 prometheus-app.0.7 psq.0.2.0 ptime.0.8.5 re.1.9.0 res.5.0.1 result.1.5 routes.0.8.0 rresult.0.6.0 seq.base session.0.4.1 session-cohttp.0.4.1 session-cohttp-lwt.0.4.1 sexplib.v0.14.0 sexplib0.v0.14.0 sqlite3.5.0.2 stdint.0.6.0 stdio.v0.14.0 stdlib-shims.0.1.0 stringext.1.6.0 tls.0.12.4 tls-mirage.0.12.4 topkg.1.0.2 tyxml.4.4.0 uchar.0.0.2 uri.3.1.0 uri-sexp.3.1.0 uuidm.0.9.7 uutf.1.0.2 x509.0.11.2 yojson.1.7.0 zarith.1.9.1" ::
      run "opam depext --update -y ocaml-ci-web.dev ocaml-ci-solver.dev ocaml-ci-service.dev ocaml-ci-client.dev ocaml-ci-api.dev $DEPS" ::
      run "opam install $DEPS" ::
      copy ["."] "/src/" ::
      run "opam exec -- dune build @install @runtest && rm -rf _build" ::
      []
  }
