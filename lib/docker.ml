let export_env ~sw ~process base : Config.env =
  let env =
    Os.pread ~sw ~process ["docker"; "image"; "inspect";
              "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
              "--"; base] 
  in
  String.split_on_char '\x00' env
  |> List.filter_map (function
      | "\n" -> None
      | kv ->
        match Astring.String.cut ~sep:"=" kv with
        | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
        | Some _ as pair -> pair
    )

let with_container ~sw ~process ~log base fn =
  let cid = Os.with_pipe_from_child ~sw (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      (* let copy () = Build_log.copy ~src:(r :> <Eio.Flow.source; Eio_unix.unix_fd>) ~dst:log in *)
      (* ignore (failwith "with container"); *)
      let cid = Os.pread ~sw ~process ~stderr:(w :> Eio.Flow.sink) ["docker"; "create"; "--"; base] in
      (* copy (); *)
      String.trim cid
    )
  in
  Fun.protect
    (fun () -> fn cid)
    ~finally:(fun () -> Os.exec ~sw ~process ["docker"; "rm"; "--"; cid])


let fetch ~sw ~process ~log ~rootfs base =
    with_container ~sw ~process ~log base (fun cid ->
      Os.with_pipe_between_children ~sw @@ fun ~r ~w ->
      Os.exec ~sw ~process ~stdout:(w :> Eio.Flow.sink) ["docker"; "export"; "--"; cid];
      Os.sudo ~sw ~process ~stdin:(r :> Eio.Flow.source) ["tar"; "-C"; rootfs; "-xf"; "-"]
    );
    export_env ~sw ~process base