open Sexplib.Conv

let ( / ) = Filename.concat

type layerinfo = {
  snapshot_key : string;
  source : string;
  parent_layer_paths : string list;
} [@@deriving sexp]

let layerinfo_path dir = dir / "layerinfo"

let write_layerinfo ~dir li =
  Os.write_file ~path:(layerinfo_path dir)
    (Sexplib.Sexp.to_string_hum (sexp_of_layerinfo li) ^ "\n")

let read_layerinfo dir =
  layerinfo_of_sexp (Sexplib.Sexp.load_sexp (layerinfo_path dir))

(* Parse the JSON output of `ctr snapshot prepare --mounts`.
   Format:
   [{"Type":"windows-layer","Source":"C:\\...\\snapshots\\N","Target":"",
     "Options":["rw","parentLayerPaths=[\"C:\\\\...\\\\snapshots\\\\M\"]"]}]
   Returns (source_path, parent_layer_paths). *)
let parse_mount_json output =
  try
    let json = Yojson.Safe.from_string (String.trim output) in
    let open Yojson.Safe.Util in
    match to_list json with
    | [] -> ("", [])
    | mount :: _ ->
      let source = mount |> member "Source" |> to_string in
      let options = mount |> member "Options" |> to_list |> List.map to_string in
      let parents =
        List.find_map (fun opt ->
          match Astring.String.cut ~sep:"parentLayerPaths=" opt with
          | Some (_, json_str) ->
            (try
               let arr = Yojson.Safe.from_string json_str in
               Some (to_list arr |> List.map to_string)
             with _ -> None)
          | None -> None
        ) options
        |> Option.value ~default:[]
      in
      (source, parents)
  with _ -> ("", [])
