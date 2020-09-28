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

let pp_rev_path = Fmt.(list ~sep:(const string Filename.dir_sep) string)

let platform_dir_sep =
  assert (String.length Filename.dir_sep = 1);
  Filename.dir_sep.[0]

let rec check_path ~acc base = function
  | [] -> Ok acc
  | ("" | ".") :: xs -> check_path ~acc base xs
  | ".." :: _ -> Error (`Msg "Can't use .. in source paths!")
  | x :: _ when String.contains x platform_dir_sep ->
    Fmt.error_msg "Can't use platform directory separator in path component: %S" x
  | x :: xs ->
    let path = base / x in
    let acc = x :: acc in
    match Unix.lstat path with
    | exception Unix.Unix_error(Unix.ENOENT, _, _) -> Error `Not_found
    | Unix.{ st_kind = S_DIR; _ } -> check_path ~acc path xs
    | Unix.{ st_kind = S_REG | S_LNK; _ } when xs = [] -> Ok acc
    | Unix.{ st_kind = S_REG; _ } -> Fmt.error_msg "Not a directory: %a" pp_rev_path acc
    | _ -> Fmt.error_msg "Not a regular file: %a" pp_rev_path acc

let generate ~exclude ~src_dir src =
  match check_path ~acc:[] src_dir (String.split_on_char '/' src) with
  | Error (`Msg m) -> Fmt.error_msg "%s (in %S)" m src
  | Error `Not_found -> Fmt.error_msg "Source path %S not found" src
  | Ok src' ->
    try
      List.rev src'
      |> String.concat Filename.dir_sep 
      |> generate ~exclude ~src_dir 
      |> Result.ok
    with Failure m ->
      Error (`Msg m)
