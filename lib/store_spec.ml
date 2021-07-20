(** Configuration information to set up a store. *)

open Lwt.Infix

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Pool *)
  | `Docker of string (* Path *)
]

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> Ok (`Zfs pool)
  | Some ("btrfs", path) -> Ok (`Btrfs path)
  | Some ("docker", path) -> Ok (`Docker path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs: or docker:")

let pp f = function
  | `Zfs pool -> Fmt.pf f "zfs:%s" pool
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Docker path -> Fmt.pf f "docker:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store = function
  | `Btrfs path ->
    Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs pool ->
    Zfs_store.create ~pool >|= fun store ->
    Store ((module Zfs_store), store)
  | `Docker path ->
     Docker_store.create path >|= fun store ->
     Store ((module Docker_store), store)
