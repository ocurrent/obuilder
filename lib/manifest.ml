let ( / ) = Filename.concat

type hash = Sha256.t

let show_hash = Sha256.to_hex
let pp_hash f t = Fmt.string f (show_hash t)

type t = [
  | `File of (string * hash)
  | `Symlink of (string * string)
  | `Dir of (string * t list)
] [@@deriving show]

let rec generate ~exclude ~src_dir src : t =
  (* TODO: sanitise src *)
  let path = src_dir / src in
  match Unix.lstat path with
  | Unix.{ st_kind = S_DIR; _ } ->
    let items = Sys.readdir path in
    Array.sort String.compare items;
    let items =
      items
      |> Array.to_list
      |> List.filter (fun x -> not (List.mem x exclude))
      |> List.map (fun item -> generate ~exclude ~src_dir (src / item))
    in
    `Dir (src, items)
  | Unix.{ st_kind = S_REG; _ } ->
    let hash = Sha256.file path in
    `File (src, hash)
  | Unix.{ st_kind = S_LNK; _ } ->
    let target = Unix.readlink path in
    `Symlink (src, target)
  | _ -> Fmt.failwith "Unsupported file type for %S" src
  | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
    Fmt.failwith "File %S not found in source directory" src
