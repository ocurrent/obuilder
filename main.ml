open Lwt.Infix

let ( / ) = Filename.concat

(*
module Store = Obuilder.Zfs_store
let store = Lwt_main.run @@ Store.create ~pool:"tank"
*)

module Sandbox = Obuilder.Runc_sandbox

let env =
  ("OPAMYES", "true") ::
  Obuilder.Context.default_env

let build_in (type s) (module Store : Obuilder.S.STORE with type t = s) (store : s) spec src_dir =
  let module Builder = Obuilder.Builder(Store)(Sandbox) in
  let sandbox = Sandbox.create ~runc_state_dir:(Store.state_dir store / "runc") in
  Fmt_tty.setup_std_outputs ();
  let spec = Obuilder.Spec.stage_of_sexp (Sexplib.Sexp.load_sexp spec) in
  (* assert (spec = Obuilder.Spec.stage_of_sexp (Obuilder.Spec.sexp_of_stage spec)); *)
  let builder = Builder.v ~store ~sandbox in
  let context = Obuilder.Context.v ~env ~src_dir () in
  Builder.build builder context spec >>= function
  | Ok x ->
    Fmt.pr "Got: %a@." Store.ID.pp x;
    Lwt.return_unit
  | Error `Cant_happen -> assert false

let build store spec src_dir =
  Lwt_main.run @@ begin
    match store with
    | `Btrfs path ->
      let store = Obuilder.Btrfs_store.create path in
      build_in (module Obuilder.Btrfs_store) store spec src_dir
    | `Zfs pool ->
      Obuilder.Zfs_store.create ~pool >>= fun store ->
      build_in (module Obuilder.Zfs_store) store spec src_dir
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

let build =
  let doc = "Build a spec file." in
  Term.(const build $ store $ spec_file $ src_dir),
  Term.info "build" ~doc

let cmds = [build]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () = term_exit @@ Term.eval_choice default_cmd cmds
