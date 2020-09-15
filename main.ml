open Lwt.Infix

let ( / ) = Filename.concat

module Store = Obuilder.Btrfs_store
let store = Store.create "/var/lib/docker/tal/"

(*
module Store = Obuilder.Zfs_store
let store = Lwt_main.run @@ Store.create ~pool:"tank"
*)

module Sandbox = Obuilder.Runc_sandbox
let sandbox = Sandbox.create ~runc_state_dir:(Store.state_dir store / "runc")

module Builder = Obuilder.Builder(Store)(Sandbox)

let context = "/var/lib/docker/tal/context"

let env =
  ("OPAMYES", "true") ::
  Obuilder.Context.default_env

let build spec =
  Fmt_tty.setup_std_outputs ();
  let spec = Obuilder.Spec.stage_of_sexp (Sexplib.Sexp.load_sexp spec) in
  (* assert (spec = Obuilder.Spec.stage_of_sexp (Obuilder.Spec.sexp_of_stage spec)); *)
  Lwt_main.run begin
    let builder = Builder.v ~store ~sandbox in
    let context = Obuilder.Context.v ~env ~src_dir:context () in
    Builder.build builder context spec >>= function
    | Ok x ->
      Fmt.pr "Got: %a@." Store.ID.pp x;
      Lwt.return_unit
    | Error `Cant_happen -> assert false
  end

open Cmdliner

let spec_file =
  Arg.required @@
  Arg.pos 0 Arg.(some file) None @@
  Arg.info
    ~doc:"Path of build spec file"
    ~docv:"FILE"
    []

let build =
  let doc = "Build a spec file." in
  Term.(const build $ spec_file),
  Term.info "build" ~doc

let cmds = [build]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () = term_exit @@ Term.eval_choice default_cmd cmds
