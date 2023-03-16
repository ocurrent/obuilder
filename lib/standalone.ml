open Lwt.Infix

let ( / ) = Filename.concat

let ( >>!= ) = Lwt_result.bind

(* Parse a manifest.json file obtained with download-frozen-image,
   and return:
   - a list of the layers of the image, in reverse order.
   - the image environment, as a list of VAR=VALUE strings. *)
let parse_manifest basedir =
  let open Yojson.Safe in
  let open Yojson.Safe.Util in
  let jsonfile = basedir / "manifest.json" in
  let json = from_file jsonfile in
  let json =
    match json |> to_list with
    | [ json ] -> json
    | [] ->
      failwith "Empty manifest.json"
    | exception Type_error (msg, _) ->
      Fmt.failwith "Invalid format of manifest.json (%s)" msg
    | _ ->
      failwith "Invalid format of manifest.json"
  in
  let layers =
    try
      json |> member "Layers" |> to_list |> filter_string
    with Type_error (msg, _) ->
      Fmt.failwith "Invalid format of 'Layers' within manifest.json (%s)" msg
  in
  let configfile =
    try
      json |> member "Config" |> to_string
    with Type_error (msg, _) ->
      Fmt.failwith "Invalid format of 'Config' within manifest.json (%s)" msg
  in
  let configfile = basedir / configfile in
  let json = from_file configfile in
  let env =
    try
      let config = json |> member "config" in
      let env = config |> member "Env" |> to_list in
      List.rev_map
        (function
         | `String s ->
           (match Astring.String.cut ~sep:"=" s with
            | None -> Fmt.failwith "Invalid environment stanza %S in Docker image (should be 'K=V')" s
            | Some pair -> pair)
         | _ -> failwith "Invalid json type in environment")
      env
    with Type_error (msg, _) ->
      Fmt.failwith "Invalid format of configuration json (%s)" msg
  in
  List.rev layers, env

(* Extract a given layer (relative to srcdir) into destdir. *)
let extract_layer destdir srcdir layer =
  let srcpath = srcdir / layer in
  Os.sudo [ "tar" ; "-C"; destdir ; "-xpf"; srcpath ]

(* Normalize a docker image name to work around download script shortcomings.

   The download scripts supposedly accepts image names with the following
   pattern:
     image[:tag][@digest]
   but due to incorrect string parsing, if the tag is missing and the digest
   contains a colon (which is likely to occur if there is an explicit
   digest algorithm name provided, as in `@sha256:...', then the digest
   will be used as the tag, and download is very likely to fail as no such
   tag will exist.

   This routine attemps to detect this, and forces a `:latest' tag if none
   is provided. *)
let normalize s =
  let colon = String.index_opt s ':' in
  let atsign = String.index_opt s '@' in
  match colon, atsign with
  | _, None -> s (* no digest *)
  | Some pos1, Some pos2 when pos1 < pos2 -> s (* tag present *)
  | _, Some pos2 ->
    let len = String.length s in
    let image = String.sub s 0 pos2 in
    let digest = String.sub s pos2 (len - pos2) in
    image ^ ":latest" ^ digest

let invoke_fetcher log tmpdir base =
  Os.with_pipe_from_child (fun ~r ~w ->
    let pp f = Fmt.string f "download frozen image" in
    let copy = Build_log.copy ~src:r ~dst:log in
    let stdout = `FD_move_safely w in
    let stderr = stdout in
    Os.exec_result ~pp ~stdout ~stderr
      ["download-frozen-image-v2.sh"; tmpdir; base ] >>!= fun () ->
    Lwt_result.ok copy) >>= function
    | Ok () -> Lwt.return ()
    | Error (`Msg m) -> Lwt.fail (failwith m)

let fetch ~log ~rootfs base =
  let base = normalize base in
  Lwt.catch
    (fun () ->
      Lwt_io.with_temp_dir
        (fun tmpdir ->
         invoke_fetcher log tmpdir base >>= fun () ->
         let layers, env = parse_manifest tmpdir in
         let extracters = List.rev_map (extract_layer rootfs tmpdir) layers in
         Lwt.join extracters >>= fun () ->
         Lwt.return env))
    (function
     | Sys_error s ->
       Fmt.failwith "Standalone fetcher encountered a system error: %s" s
     | e -> Lwt.fail e)
