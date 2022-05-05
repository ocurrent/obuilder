(** Configuration information to set up a store. *)

open Lwt.Infix

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Path with pool at end *)
  | `Rsync of string  (* Path for the root of the store *)
]

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", path) -> Ok (`Zfs path)
  | Some ("btrfs", path) -> Ok (`Btrfs path)
  | Some ("rsync", path) -> Ok (`Rsync path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs:")

let pp f = function
  | `Zfs path -> Fmt.pf f "zfs:%s" path
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Rsync path -> Fmt.pf f "rsync:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store = function
  | `Btrfs path ->
    Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs path ->
    Zfs_store.create ~path >|= fun store ->
    Store ((module Zfs_store), store)
  | `Rsync path ->
    Rsync_store.create ~path >|= fun store ->
    Store ((module Rsync_store), store)
