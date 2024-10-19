(** Configuration information to set up a store. *)

open Lwt.Infix

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Path with pool at end *)
  | `Rsync of (string * Rsync_store.mode)  (* Path for the root of the store *)
  | `Xfs of string    (* Path *)
  | `Overlayfs of string (* Path *)
  | `Docker of string (* Path *)
  | `Qemu of string   (* Path *)
]

let is_absolute path = not (Filename.is_relative path)

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> Ok (`Zfs pool)
  | Some ("btrfs", path) when is_absolute path -> Ok (`Btrfs path)
  | Some ("rsync", path) when is_absolute path -> Ok (`Rsync path)
  | Some ("xfs", path) when is_absolute path -> Ok (`Xfs path)
  | Some ("overlayfs", path) when is_absolute path -> Ok (`Overlayfs path)
  | Some ("docker", path) -> Ok (`Docker path)
  | Some ("qemu", path) -> Ok (`Qemu path)
  | _ -> Error (`Msg "Store must start with zfs:, btrfs:/, rsync:/, xfs:/, qemu:/ or overlayfs:")

let pp f = function
  | `Zfs path -> Fmt.pf f "zfs:%s" path
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Rsync path -> Fmt.pf f "rsync:%s" path
  | `Xfs path -> Fmt.pf f "xfs:%s" path
  | `Overlayfs path -> Fmt.pf f "overlayfs:%s" path
  | `Docker path -> Fmt.pf f "docker:%s" path
  | `Qemu path -> Fmt.pf f "qemu:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store = function
  | `Btrfs path ->
    `Native, Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs path ->
    `Native, Zfs_store.create ~path >|= fun store ->
    Store ((module Zfs_store), store)
  | `Rsync (path, rsync_mode) ->
    `Native, Rsync_store.create ~path ~mode:rsync_mode () >|= fun store ->
    Store ((module Rsync_store), store)
  | `Xfs path ->
    `Native, Xfs_store.create ~path >|= fun store ->
    Store ((module Xfs_store), store)
  | `Overlayfs path ->
    `Native, Overlayfs_store.create ~path >|= fun store ->
    Store ((module Overlayfs_store), store)
  | `Docker path ->
    `Docker, Docker_store.create path >|= fun store ->
    Store ((module Docker_store), store)
  | `Qemu root ->
    `Qemu, Qemu_store.create ~root >|= fun store ->
    Store ((module Qemu_store), store)

open Cmdliner

let store_t = Arg.conv (of_string, pp)

let store ?docs names =
  Arg.opt Arg.(some store_t) None @@
  Arg.info
    ~doc:"$(docv) must be one of $(b,btrfs:/path), $(b,rsync:/path), $(b,xfs:/path), $(b,overlayfs:/path), $(b,zfs:pool), $(b,qmu:/path) or $(b,docker:path) for the OBuilder cache."
    ~docv:"STORE"
    ?docs
    names

let rsync_mode_opt =
  let options =
    [("copy", Rsync_store.Copy);
     ("hardlink", Rsync_store.Hardlink);
     ("hardlink_unsafe", Rsync_store.Hardlink_unsafe)]
  in
  Arg.opt Arg.(some (enum options)) None @@
    Arg.info
      ~doc:(Printf.sprintf "Optimize for speed or low disk usage. $(docv) must be %s."
              (Arg.doc_alts_enum options))
      ~docv:"RSYNC_MODE"
      ~docs:"RSYNC STORE"
      ["rsync-mode"]

let rsync_mode =
  Arg.value @@ rsync_mode_opt

(** Transform a [store] and [rsync-mode] into a validated combination.

    For example an rsync store must supply an rsync-mode.
 *)
let of_t store rsync_mode =
  match store, rsync_mode with
  | Some (`Rsync path), Some rsync_mode -> `Rsync (path, rsync_mode)
  | Some (`Rsync _path), None -> failwith "Store rsync:/ must supply an rsync-mode"
  | Some (`Btrfs path), None -> (`Btrfs path)
  | Some (`Zfs path), None -> (`Zfs path)
  | Some (`Xfs path), None -> (`Xfs path)
  | Some (`Overlayfs path), None -> (`Overlayfs path)
  | Some (`Docker path), None -> (`Docker path)
  | Some (`Qemu path), None -> (`Qemu path)
  | _, _ -> failwith "Store type required must be one of btrfs:/path, rsync:/path, xfs:/path, zfs:pool, qemu:/path or docker:path for the OBuilder cache."

(** Parse cli arguments for t *)
let v =
  Term.(const of_t
        $ Arg.value @@ store ["store"]
        $ Arg.value @@ rsync_mode_opt)

(** Parse cli arguments for t and initialise a [store]. *)
let cmdliner =
  Term.(const to_store $ v)
