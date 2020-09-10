open Lwt.Infix

module Store = Btrfs_store
module Builder = Build.Make(Store)

let store = Store.create "/var/lib/docker/tal/"
let runc_state_dir = "/var/lib/docker/tal/state"
let context = "/var/lib/docker/tal/context"

let env =
  ("OPAMYES", "true") ::
  Build.Context.default_env

let () =
  Lwt_main.run begin
    let builder = Builder.v ~store ~runc_state_dir in
    let context = Build.Context.v ~env ~src_dir:context () in
    Builder.build builder context Example.dockerfile >>= function
    | Ok x ->
      Fmt.pr "Got: %a@." Store.ID.pp x;
      Lwt.return_unit
    | Error `Cant_happen -> assert false
  end
