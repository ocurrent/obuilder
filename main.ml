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

let create_builder (type s) (module Store : Obuilder.S.STORE with type t = s) (store : s) =
  let module Builder = Obuilder.Builder(Store)(Sandbox) in
  let sandbox = Sandbox.create ~runc_state_dir:(Store.state_dir store / "runc") in
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let create_builder = function
  | `Btrfs path ->
    let store = Obuilder.Btrfs_store.create path in
    Lwt.return @@ create_builder (module Obuilder.Btrfs_store) store
  | `Zfs pool ->
    Obuilder.Zfs_store.create ~pool >|= fun store ->
    create_builder (module Obuilder.Zfs_store) store

let build store spec src_dir =
  Lwt_main.run begin
    create_builder store >>= fun (Builder ((module Builder), builder)) ->
    let spec = Obuilder.Spec.stage_of_sexp (Sexplib.Sexp.load_sexp spec) in
    (* assert (spec = Obuilder.Spec.stage_of_sexp (Obuilder.Spec.sexp_of_stage spec)); *)
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

let cut ~sep s =
  String.index_opt s sep |> Option.map @@ fun i ->
  String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)

let store_t =
  let parse s =
    match cut s ~sep:':' with
    | Some ("zfs", pool) -> Ok (`Zfs pool)
    | Some ("btrfs", path) -> Ok (`Btrfs path)
    | _ -> Error (`Msg "Store must start with zfs: or btrfs:")
  in
  let pp f = function
    | `Zfs pool -> Fmt.pf f "zfs:%s" pool
    | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  in
  Arg.conv (parse, pp)

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

let cmds = [build; delete]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () =
  Fmt_tty.setup_std_outputs ();
  term_exit @@ Term.eval_choice default_cmd cmds
