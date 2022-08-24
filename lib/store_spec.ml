(** Configuration information to set up a store. *)

open Lwt.Infix

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Pool *)
  | `Rsync of string  (* Path for the root of the store *)
  | `Docker of string (* Path *)
]

let is_absolute path = not (Filename.is_relative path)

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> Ok (`Zfs pool)
  | Some ("btrfs", path) when is_absolute path -> Ok (`Btrfs path)
  | Some ("rsync", path) when is_absolute path -> Ok (`Rsync path)
  | Some ("docker", path) -> Ok (`Docker path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs:/ or rsync:/")

let pp f = function
  | `Zfs pool -> Fmt.pf f "zfs:%s" pool
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Rsync path -> Fmt.pf f "rsync:%s" path
  | `Docker path -> Fmt.pf f "docker:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store rsync_mode = function
  | `Btrfs path ->
    `Runc, Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs pool ->
    `Runc, Zfs_store.create ~pool >|= fun store ->
    Store ((module Zfs_store), store)
  | `Rsync path ->
    `Runc, Rsync_store.create ~path ~mode:rsync_mode () >|= fun store ->
    Store ((module Rsync_store), store)
  | `Docker path ->
    `Docker, Docker_store.create path >|= fun store ->
    Store ((module Docker_store), store)

open Cmdliner

let store_t = Arg.conv (of_string, pp)

let store names =
  Arg.opt Arg.(some store_t) None @@
  Arg.info
    ~doc:"$(docv) must be one of $(b,btrfs:/path), $(b,rsync:/path), $(b,zfs:pool) or $(b,docker:path) for the OBuilder cache."
    ~docv:"STORE"
    names

let rsync_mode =
  let options =
    [("copy", Rsync_store.Copy);
     ("hardlink", Rsync_store.Hardlink);
     ("hardlink_unsafe", Rsync_store.Hardlink_unsafe)]
  in
  Arg.value @@
  Arg.opt (Arg.enum options) Rsync_store.Copy @@
  Arg.info
    ~doc:(Printf.sprintf "Optimize for speed or low disk usage. $(docv) must be one of %s."
            (Arg.doc_alts_enum options))
    ~docv:"RSYNC_MODE"
    ["rsync-mode"]

(** A Cmdliner term where the store is required. *)
let cmdliner =
  Term.(const to_store $ rsync_mode $ (Arg.required @@ (store ["store"])))

(** A Cmdliner term where the store is optional. *)
let cmdliner_opt =
  let make rsync_mode = function
    | None -> None
    | Some store -> Some (to_store rsync_mode store)
  in
  Term.(const make $ rsync_mode $ (Arg.value @@ (store ["obuilder-store"])))
