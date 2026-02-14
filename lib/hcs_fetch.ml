open Lwt.Infix

let ( / ) = Filename.concat

let ctr_exec args =
  let pp f = Os.pp_cmd f ("", "ctr" :: args) in
  Os.exec_result ~pp ("ctr" :: args)

let ctr_pread args =
  if Sys.win32 then
    Os.win32_pread ("ctr" :: args)
  else
    let pp f = Os.pp_cmd f ("", "ctr" :: args) in
    Os.pread_result ~pp ("ctr" :: args)

(* Parse the config digest from `ctr images inspect` tree output.
   Look for lines like: "application/vnd.docker.container.image.v1+json @sha256:..." *)
let parse_config_digest output =
  let lines = String.split_on_char '\n' output in
  List.find_map (fun line ->
    if Astring.String.is_infix ~affix:"container.image.v1+json" line then
      match Astring.String.cut ~sep:"@" line with
      | Some (_, digest) ->
        let digest = String.trim digest in
        (* Extract just the digest, removing any trailing info like "(123 bytes)" *)
        (match Astring.String.cut ~sep:" " digest with
         | Some (d, _) -> Some d
         | None -> Some digest)
      | None -> None
    else None
  ) lines

let parse_env_from_config output =
  try
    let json = Yojson.Safe.from_string output in
    let open Yojson.Safe.Util in
    let config = json |> member "config" in
    let env_list = config |> member "Env" |> to_list |> List.map to_string in
    List.filter_map (fun s ->
      match String.index_opt s '=' with
      | Some i ->
        let key = String.sub s 0 i in
        let value = String.sub s (i + 1) (String.length s - i - 1) in
        Some (key, value)
      | None -> None
    ) env_list
  with _ -> []


(* Parse the chain ID from `ctr images pull --print-chainid --local` output.
   The output contains a line like: "image chain ID: sha256:abc123..." *)
let parse_chain_id output =
  let lines = String.split_on_char '\n' output in
  List.find_map (fun line ->
    match Astring.String.cut ~sep:"image chain ID: " line with
    | Some (_, chain_id) -> Some (String.trim chain_id)
    | None -> None
  ) lines

(* Normalize image reference for containerd.
   Docker Hub images need docker.io/ prefix:
   - "ubuntu:latest" -> "docker.io/library/ubuntu:latest"
   - "ocaml/opam:tag" -> "docker.io/ocaml/opam:tag"
   - "mcr.microsoft.com/..." -> unchanged (already has registry)
   - "docker.io/..." -> unchanged *)
let normalize_image_ref image =
  if String.contains image '/' then
    (* Has a slash - check if it starts with a registry *)
    let first_part =
      match String.index_opt image '/' with
      | Some i -> String.sub image 0 i
      | None -> image
    in
    (* If first part contains a dot or colon, it's a registry *)
    if String.contains first_part '.' || String.contains first_part ':' then
      image  (* Already has registry prefix *)
    else
      "docker.io/" ^ image  (* Docker Hub user/repo format *)
  else
    (* No slash - it's a Docker Hub library image *)
    "docker.io/library/" ^ image

let fetch ~log:(_log : Build_log.t) ~root:(_root : string) ~rootfs base : Config.env Lwt.t =
  let image = normalize_image_ref base in
  let hash = Sha256.to_hex (Sha256.string base) in
  let key = "obuilder-base-" ^ hash in
  (* Pull the image — on Windows containerd, pull also unpacks layers *)
  Log.info (fun f -> f "HCS fetch: pulling image %s (from %s)" image base);
  let platform = ["--platform"; "windows/amd64"] in
  (ctr_exec (["images"; "pull"] @ platform @ [image]) >>= function
   | Ok () -> Log.info (fun f -> f "HCS fetch: pull succeeded"); Lwt.return_unit
   | Error (`Msg m) -> Fmt.failwith "Failed to pull image %s: %s" image m)
  >>= fun () ->
  (* Get the image's chain ID (the snapshot key for the top layer).
     Using --local makes this fast since the image is already pulled. *)
  Log.info (fun f -> f "HCS fetch: getting chain ID");
  (ctr_pread (["images"; "pull"; "--print-chainid"; "--local"] @ platform @ [image]) >>= function
   | Ok output ->
     Log.info (fun f -> f "HCS fetch: got chainid output");
     (match parse_chain_id output with
      | Some chain_id -> Log.info (fun f -> f "HCS fetch: chain ID = %s" chain_id); Lwt.return chain_id
      | None -> Fmt.failwith "Could not find chain ID for image %s" image)
   | Error (`Msg m) ->
     Fmt.failwith "Failed to get chain ID for image %s: %s" image m)
  >>= fun chain_id ->
  (* Clean up any existing snapshots with this key first (for idempotency).
     Remove any snapshots that depend on our key, then remove our key itself. *)
  Log.info (fun f -> f "HCS fetch: cleaning up any existing snapshots for %s" key);
  let committed_key = key ^ "-committed" in
  (* Use ctr snapshot ls and parse to find snapshots that have our committed key as parent *)
  (ctr_pread ["snapshot"; "ls"] >>= function
   | Ok output ->
     let lines = String.split_on_char '\n' output in
     let children = lines |> List.filter_map (fun line ->
       (* Format: KEY\s+PARENT\s+KIND *)
       let parts = Astring.String.cuts ~empty:false ~sep:" " (String.trim line) in
       match parts with
       | child :: parent :: _ when parent = committed_key -> Some child
       | _ -> None
     ) in
     Lwt_list.iter_s (fun child ->
       Log.info (fun f -> f "HCS fetch: removing child snapshot %s" child);
       ctr_exec ["snapshot"; "rm"; child] >>= fun _ -> Lwt.return_unit
     ) children
   | Error _ -> Lwt.return_unit)
  >>= fun () ->
  (* Now remove the main snapshots *)
  (ctr_exec ["snapshot"; "rm"; key] >>= function
   | Ok () -> Log.info (fun f -> f "HCS fetch: removed existing snapshot"); Lwt.return_unit
   | Error (`Msg _) -> Log.info (fun f -> f "HCS fetch: no existing snapshot to remove"); Lwt.return_unit)
  >>= fun () ->
  (ctr_exec ["snapshot"; "rm"; committed_key] >>= function
   | Ok () -> Log.info (fun f -> f "HCS fetch: removed existing committed snapshot"); Lwt.return_unit
   | Error (`Msg m) -> Log.info (fun f -> f "HCS fetch: could not remove committed snapshot: %s" m); Lwt.return_unit)
  >>= fun () ->
  (* Prepare a writable snapshot from the image's top layer *)
  Log.info (fun f -> f "HCS fetch: preparing snapshot %s from %s" key chain_id);
  (ctr_pread ["snapshot"; "prepare"; "--mounts"; key; chain_id] >>= function
   | Ok mounts_json ->
     Log.info (fun f -> f "HCS fetch: snapshot prepared, parsing mount json");
     let source, parent_layer_paths = Hcs.parse_mount_json mounts_json in
     Log.info (fun f -> f "HCS fetch: source=%s, parents=%d" source (List.length parent_layer_paths));
     Log.info (fun f -> f "HCS fetch: writing layerinfo to %s" rootfs);
     Hcs.write_layerinfo ~dir:rootfs { snapshot_key = key; source; parent_layer_paths } >>= fun () ->
     Log.info (fun f -> f "HCS fetch: layerinfo written");
     Lwt.return_unit
   | Error (`Msg m) ->
     Fmt.failwith "Failed to prepare snapshot for base %s: %s" base m)
  >>= fun () ->
  (* Get environment variables from the image config.
     First get the config digest from inspect, then get the config content. *)
  Log.info (fun f -> f "HCS fetch: getting image config");
  (ctr_pread ["images"; "inspect"; image] >>= function
   | Ok inspect_output ->
     (match parse_config_digest inspect_output with
      | Some config_digest ->
        Log.info (fun f -> f "HCS fetch: config digest = %s" config_digest);
        ctr_pread ["content"; "get"; config_digest] >>= (function
        | Ok config_json ->
          Log.info (fun f -> f "HCS fetch: got config, parsing env");
          Lwt.return (parse_env_from_config config_json)
        | Error (`Msg m) ->
          Log.warn (fun f -> f "HCS fetch: failed to get config content: %s" m);
          Lwt.return [])
      | None ->
        Log.warn (fun f -> f "HCS fetch: could not find config digest in inspect output");
        Lwt.return [])
   | Error (`Msg m) ->
     Log.warn (fun f -> f "HCS fetch: failed to inspect image: %s" m);
     Lwt.return [])
  >>= fun env ->
  Log.info (fun f -> f "HCS fetch: done, got %d env vars" (List.length env));
  Lwt.return env
