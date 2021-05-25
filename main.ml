open Lwt.Infix

let ( / ) = Filename.concat

module Native_sandbox = Obuilder.Native_sandbox
module Docker_sandbox = Obuilder.Docker_sandbox
module Docker_store = Obuilder.Docker_store
module Docker_extract = Obuilder.Docker_extract
module Store_spec = Obuilder.Store_spec

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output -> output_string stdout msg; flush stdout

let create_builder store_spec conf =
  store_spec >>= fun (Store_spec.Store ((module Store), store)) ->
  let module Builder = Obuilder.Builder (Store) (Native_sandbox) (Docker_extract) in
  Native_sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let create_docker_builder store_spec conf =
  store_spec >>= fun (Store_spec.Store ((module Store), store)) ->
  let module Builder = Obuilder.Docker_builder (Store) in
  Docker_sandbox.create conf >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let select_backend (sandbox, store_spec) native_conf docker_conf =
  match sandbox with
  | `Native -> create_builder store_spec native_conf
  | `Docker -> create_docker_builder store_spec docker_conf

let build () store spec native_conf docker_conf src_dir secrets =
  Lwt_main.run begin
    select_backend store native_conf docker_conf
    >>= fun (Builder ((module Builder), builder)) ->
    Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ fun () ->
    let spec =
      try Obuilder.Spec.t_of_sexp (Sexplib.Sexp.load_sexp spec)
      with Failure msg ->
        print_endline msg;
        exit 1
    in
    let secrets = List.map (fun (id, path) -> id, read_whole_file path) secrets in
    let context = Obuilder.Context.v ~log ~src_dir ~secrets () in
    Builder.build builder context spec >>= function
    | Ok x ->
      Fmt.pr "Got: %S@." (x :> string);
      Lwt.return_unit
    | Error `Cancelled ->
      Fmt.epr "Cancelled at user's request@.";
      exit 1
    | Error (`Msg m) ->
      Fmt.epr "Build step failed: %s@." m;
      exit 1
  end

let healthcheck () store native_conf docker_conf =
  Lwt_main.run begin
    select_backend store native_conf docker_conf
    >>= fun (Builder ((module Builder), builder)) ->
    Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ fun () ->
    Builder.healthcheck builder >|= function
    | Error (`Msg m) ->
      Fmt.epr "Healthcheck failed: %s@." m;
      exit 1
    | Ok () ->
      Fmt.pr "Healthcheck passed@."
  end

let delete () store native_conf docker_conf id =
  Lwt_main.run begin
    select_backend store native_conf docker_conf
    >>= fun (Builder ((module Builder), builder)) ->
    Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ fun () ->
    Builder.delete builder id ~log:(fun id -> Fmt.pr "Removing %s@." id)
  end

let clean () store native_conf docker_conf =
  Lwt_main.run begin
    select_backend store native_conf docker_conf
    >>= fun (Builder ((module Builder), builder)) ->
    Fun.flip Lwt.finalize (fun () -> Builder.finish builder) @@ begin fun () ->
      let now = Unix.(gmtime (gettimeofday ())) in
      Builder.prune builder ~before:now max_int ~log:(fun id -> Fmt.pr "Removing %s@." id)
    end >|= fun n ->
    Fmt.pr "Removed %d items@." n
  end

let dockerfile () buildkit escape spec =
  Sexplib.Sexp.load_sexp spec
  |> Obuilder_spec.t_of_sexp
  |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit ~os:escape
  |> print_endline

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Obuilder.log_src level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let spec_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of build spec file."
    ~docv:"FILE"
    ["f"]

let src_dir =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"Directory containing the source files."
    ~docv:"DIR"
    []

let store = Store_spec.cmdliner

let id =
  Arg.required @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"The $(i,ID) of a build within the store."
    ~docv:"ID"
    []

let secrets =
  (Arg.value @@
   Arg.(opt_all (pair ~sep:':' string file)) [] @@
   Arg.info
     ~doc:"Provide a secret under the form $(b,id:file)."
     ~docv:"SECRET"
     ["secret"])

let build =
  let doc = "Build a spec file." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info
    Term.(const build $ setup_log $ store $ spec_file $ Native_sandbox.cmdliner
          $ Docker_sandbox.cmdliner $ src_dir $ secrets)

let delete =
  let doc = "Recursively delete a cached build result." in
  let info = Cmd.info "delete" ~doc in
  Cmd.v info
    Term.(const delete $ setup_log $ store $ Native_sandbox.cmdliner
          $ Docker_sandbox.cmdliner $ id)

let clean =
  let doc = "Clean all cached build results." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info
    Term.(const clean $ setup_log $ store $ Native_sandbox.cmdliner
          $ Docker_sandbox.cmdliner)

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Output extended BuildKit syntax."
    ["buildkit"]

let escape =
  let styles = [("unix", `Unix); ("windows", `Windows)] in
  let doc = Arg.doc_alts_enum styles |> Printf.sprintf "Dockerfile escape style, must be %s." in
  Arg.value @@
  Arg.opt Arg.(enum styles) (if Sys.unix then `Unix else `Windows) @@
  Arg.info ~doc
    ~docv:"STYLE"
    ["escape"]

let dockerfile =
  let doc = "Convert a spec to Dockerfile format." in
  let info = Cmd.info ~doc "dockerfile" in
  Cmd.v info
    Term.(const dockerfile $ setup_log $ buildkit $ escape $ spec_file)

let healthcheck =
  let doc = "Perform a self-test" in
  let info = Cmd.info "healthcheck" ~doc in
  Cmd.v info
    Term.(const healthcheck $ setup_log $ store $ Native_sandbox.cmdliner
          $ Docker_sandbox.cmdliner)

let cmds = [build; delete; clean; dockerfile; healthcheck]

let () =
  let doc = "a command-line interface for OBuilder" in
  let info = Cmd.info ~doc "obuilder" in
  exit (Cmd.eval @@ Cmd.group info cmds)
