open Lwt.Infix

let export_env base : Config.env Lwt.t =
  Os.pread ["docker"; "image"; "inspect";
            "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
            "--"; base] >|= fun env ->
  String.split_on_char '\x00' env
  |> List.filter_map (function
      | "\n" -> None
      | kv ->
        match Astring.String.cut ~sep:"=" kv with
        | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
        | Some _ as pair -> pair
    )

let with_container ~log base fn =
  Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = Build_log.copy ~src:r ~dst:log in
      Os.pread ~stderr:(`FD_move_safely w) ["docker"; "create"; "--"; base] >>= fun cid ->
      copy >|= fun () ->
      String.trim cid
    ) >>= fun cid ->
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> Os.exec ~stdout:`Dev_null ["docker"; "rm"; "--"; cid])


let fetch ~log ~rootfs base =  
  with_container ~log base (fun cid ->
    Os.with_pipe_between_children @@ fun ~r ~w ->
    let exporter = Os.exec ~stdout:(`FD_move_safely w) ["docker"; "export"; "--"; cid] in
    let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
    exporter >>= fun () ->
    tar
  ) >>= fun () -> 
  export_env base
