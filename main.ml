open Lwt.Infix

let ( / ) = Filename.concat

module Sandbox = Obuilder.Runc_sandbox
module Fetcher = Obuilder.Docker

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output -> output_string stdout msg; flush stdout

let create_builder spec conf =
  Obuilder.Store_spec.to_store spec >>= fun (Store ((module Store), store)) ->
  let module Builder = Obuilder.Builder(Store)(Sandbox)(Fetcher) in
  Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let build () store spec conf src_dir secrets =
  Lwt_main.run begin
    create_builder store conf >>= fun (Builder ((module Builder), builder)) ->
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

let healthcheck () store conf =
  Lwt_main.run begin
    create_builder store conf >>= fun (Builder ((module Builder), builder)) ->
    Builder.healthcheck builder >|= function
    | Error (`Msg m) ->
      Fmt.epr "Healthcheck failed: %s@." m;
      exit 1
    | Ok () ->
      Fmt.pr "Healthcheck passed@."
  end

let delete () store conf id =
  Lwt_main.run begin
    create_builder store conf >>= fun (Builder ((module Builder), builder)) ->
    Builder.delete builder id ~log:(fun id -> Fmt.pr "Removing %s@." id)
  end

let dockerfile () buildkit spec =
  Sexplib.Sexp.load_sexp spec
  |> Obuilder_spec.t_of_sexp
  |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit
  |> print_endline

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Obuilder.log_src level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let spec_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of build spec file"
    ~docv:"FILE"
    ["f"]

let src_dir =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"Directory containing the source files"
    ~docv:"DIR"
    []

let store_t =
  Arg.conv Obuilder.Store_spec.(of_string, pp)

let store =
  Arg.required @@
  Arg.opt Arg.(some store_t) None @@
  Arg.info
    ~doc:"zfs:pool or btrfs:/path for build cache"
    ~docv:"STORE"
    ["store"]

let id =
  Arg.required @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"The ID of a build within the store"
    ~docv:"ID"
    []

let secrets =
  (Arg.value @@
   Arg.(opt_all (pair ~sep:':' string file)) [] @@
   Arg.info
     ~doc:"Provide a secret under the form id:file"
     ~docv:"SECRET"
     ["secret"])

let build =
  let doc = "Build a spec file." in
  Term.(const build $ setup_log $ store $ spec_file $ Sandbox.cmdliner $ src_dir $ secrets),
  Term.info "build" ~doc

let delete =
  let doc = "Recursively delete a cached build result." in
  Term.(const delete $ setup_log $ store $ Sandbox.cmdliner $ id),
  Term.info "delete" ~doc

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Output extended BuildKit syntax"
    ["buildkit"]

let dockerfile =
  let doc = "Convert a spec to Dockerfile format" in
  Term.(const dockerfile $ setup_log $ buildkit $ spec_file),
  Term.info "dockerfile" ~doc

let healthcheck =
  let doc = "Perform a self-test" in
  Term.(const healthcheck $ setup_log $ store $ Sandbox.cmdliner),
  Term.info "healthcheck" ~doc

let cmds = [build; delete; dockerfile; healthcheck]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () =
  term_exit @@ Term.eval_choice default_cmd cmds
