(** Configuration information to set up a store. *)
type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Pool *)
  | `Rsync of string  (* Path for the root of the store *)
]

let is_absolute path = not (Filename.is_relative path)

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> Ok (`Zfs pool)
  | Some ("btrfs", path) when is_absolute path -> Ok (`Btrfs path)
  | Some ("rsync", path) when is_absolute path -> Ok (`Rsync path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs:/ or rsync:/")

let pp f = function
  | `Zfs pool -> Fmt.pf f "zfs:%s" pool
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Rsync path -> Fmt.pf f "rsync:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store ~process mode = function
  | `Btrfs path ->
    let store = Btrfs_store.create process path in
    Store ((module Btrfs_store), store)
  | `Zfs pool ->
    let store = Zfs_store.create ~process ~pool in
    Store ((module Zfs_store), store)
  | `Rsync path ->
    let store = Rsync_store.create ~process ~path ~mode () in
    Store ((module Rsync_store), store)

let cmdliner process =
  let open Cmdliner in
  let store_t = Arg.conv (of_string, pp) in
  let store =
    Arg.required @@
    Arg.opt Arg.(some store_t) None @@
    Arg.info
      ~doc:"$(b,btrfs:/path) or $(b,rsync:/path) or $(b,zfs:pool) for build cache."
      ~docv:"STORE"
      ["store"]
  in
  let rsync_mode =
    let options =
      [("copy", Rsync_store.Copy);
       ("hardlink", Rsync_store.Hardlink);
       ("hardlink_unsafe", Rsync_store.Hardlink_unsafe)]
    in
    Arg.value @@
    Arg.opt (Arg.enum options) Rsync_store.Copy @@
    Arg.info
      ~doc:"$(b,copy) or $(b,hardlink), to optimize for speed or low disk usage."
      ~docv:"RSYNC_MODE"
      ["rsync-mode"]
  in
  Term.(const (to_store ~process) $ rsync_mode $ store)
