open Lwt.Infix

let export_env config : Config.env =
  Docker_hub.Config.env config |>
  List.filter_map (fun kv ->
    match Astring.String.cut ~sep:"=" kv with
    | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
    | Some _ as pair -> pair
  )

let handle_errors = function
  | Ok x -> Lwt.return x
  | Error _ -> (* TODO: pretty print the errors *)
      Lwt.fail_with "TODO"

let with_container manifest token fn =
  Lwt_io.with_temp_dir ~perm:0o700 ~prefix:"obuilder-docker-hub-" @@ fun output_file ->
  Docker_hub.fetch_rootfs ~output_file:(Fpath.v output_file) manifest token >>=
  handle_errors >>= fun () ->
  fn output_file

let fetch ~log:_ ~rootfs base =
  let name, tag, digest = Docker_hub.Image.from_string base in
  Docker_hub.Token.fetch name >>= handle_errors >>= fun token ->
  begin match digest with
  | None ->
      Docker_hub.Manifests.fetch tag token >>= handle_errors >>= fun manifests ->
      let elements = Docker_hub.Manifests.elements manifests in
      let current_platform = Lazy.force Docker_hub.Platform.current in
      let {Docker_hub.Manifests.digest; _} =
        List.find (fun {Docker_hub.Manifests.platform; _} ->
          Docker_hub.Platform.equal platform current_platform
        ) elements
      in
      Docker_hub.Manifest.fetch digest token
  | Some digest ->
      Docker_hub.Manifest.fetch digest token
  end >>= handle_errors >>= fun manifest ->
  Docker_hub.Config.fetch manifest token >>= handle_errors >>= fun config ->
  with_container manifest token (fun output_file ->
    Os.with_pipe_between_children @@ fun ~r ~w ->
    let exporter = Os.exec ~stdout:(`FD_move_safely w) ["cat"; output_file] in
    let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
    exporter >>= fun () ->
    tar
  ) >|= fun () ->
  export_env config
