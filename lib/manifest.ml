let ( / ) = Filename.concat

type t = [
  | `File of (string * string)
  | `Dir of (string * t list)
] [@@deriving show]

let rec generate ~src_dir src : t =
  (* TODO: sanitise src *)
  let path = src_dir / src in
  match Unix.lstat path with
  | Unix.{ st_kind = S_DIR; _ } ->
    let items =
      Sys.readdir path
      |> Array.to_list
      |> List.filter (( <> ) ".git")   (* TODO: .dockerignore *)
      |> List.map (fun item -> generate ~src_dir (src / item))
    in
    `Dir (src, items)
  | Unix.{ st_kind = S_REG; _ } ->
    let hash = Digest.file path in
    `File (src, hash)
  | _ -> Fmt.failwith "Unsupported file type for %S" src
  | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
    Fmt.failwith "File %S not found in source directory" src
