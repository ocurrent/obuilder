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

let () =
  Lwt_main.run begin
    let builder = Builder.v ~store ~sandbox in
    let context = Obuilder.Context.v ~env ~src_dir:context () in
    Builder.build builder context Example.dockerfile >>= function
    | Ok x ->
      Fmt.pr "Got: %a@." Store.ID.pp x;
      Lwt.return_unit
    | Error `Cant_happen -> assert false
  end
