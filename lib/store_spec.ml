(** Configuration information to set up a store. *)

open Lwt.Infix

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of string    (* Path with pool at end *)
  | `Rsync of (string * Rsync_store.mode)  (* Path for the root of the store *)
]

let is_absolute path = not (Filename.is_relative path)

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> Ok (`Zfs pool)
  | Some ("btrfs", path) when is_absolute path -> Ok (`Btrfs path)
  | Some ("rsync", path) when is_absolute path -> Ok (`Rsync path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs:/ or rsync:/")

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
  | `Rsync (path, rsync_mode) ->
    Rsync_store.create ~path ~mode:rsync_mode () >|= fun store ->
    Store ((module Rsync_store), store)

open Cmdliner

let store_t = Arg.conv (of_string, pp)

let store names =
  Arg.opt Arg.(some store_t) None @@
  Arg.info
    ~doc:"$(docv) must be one of $(b,btrfs:/path), $(b,rsync:/path) or $(b,zfs:pool) for the OBuilder cache."
    ~docv:"STORE"
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
  | _, _ -> failwith "Store type required must be one of $(b,btrfs:/path), $(b,rsync:/path) or $(b,zfs:pool) for the OBuilder cache."

(** Parse cli arguments for t *)
let v =
  Term.(const of_t
        $ Arg.value @@ store ["store"]
        $ Arg.value @@ rsync_mode_opt )

(** Parse cli arguments for t and initialise a [store]. *)
let cmdliner =
  Term.(const to_store $ v)
