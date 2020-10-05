open Lwt.Infix

let () =
  Logs.set_reporter (Logs_fmt.reporter ())

let ( / ) = Filename.concat

(*
module Store = Obuilder.Zfs_store
let store = Lwt_main.run @@ Store.create ~pool:"tank"
*)

module Sandbox = Obuilder.Runc_sandbox

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output -> output_string stdout msg; flush stdout

let create_builder spec =
  Obuilder.Store_spec.to_store spec >|= fun (Store ((module Store), store)) -> 
  let module Builder = Obuilder.Builder(Store)(Sandbox) in
  let sandbox = Sandbox.create ~runc_state_dir:(Store.state_dir store / "runc") in
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let build store spec src_dir =
  Lwt_main.run begin
    create_builder store >>= fun (Builder ((module Builder), builder)) ->
    let spec = Obuilder.Spec.stage_of_sexp (Sexplib.Sexp.load_sexp spec) in
    let context = Obuilder.Context.v ~log ~src_dir () in
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

let delete store id =
  Lwt_main.run begin
    create_builder store >>= fun (Builder ((module Builder), builder)) ->
    Builder.delete builder id ~log:(fun id -> Fmt.pr "Removing %s@." id)
  end

let dockerfile spec =
  Sexplib.Sexp.load_sexp spec
  |> Obuilder.Spec.stage_of_sexp
  |> Obuilder.Docker.dockerfile_of_spec 
  |> Dockerfile.string_of_t
  |> print_endline

open Cmdliner

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

let build =
  let doc = "Build a spec file." in
  Term.(const build $ store $ spec_file $ src_dir),
  Term.info "build" ~doc

let delete =
  let doc = "Recursively delete a cached build result." in
  Term.(const delete $ store $ id),
  Term.info "delete" ~doc

let dockerfile =
  let doc = "Convert a spec to Dockerfile format" in
  Term.(const dockerfile $ spec_file),
  Term.info "dockerfile" ~doc

let cmds = [build; delete; dockerfile]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () =
  (* Logs.(set_level (Some Info)); *)
  Fmt_tty.setup_std_outputs ();
  term_exit @@ Term.eval_choice default_cmd cmds
