let export_env ~process base : Config.env =
  let env =
    Os.pread ~process ["docker"; "image"; "inspect";
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

let with_container ~process ~log base fn =
  let cid = Os.with_pipe_from_child (fun ~r ~w ->
      Eio.Switch.run @@ fun sw ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = Eio.Fiber.fork_promise ~sw (fun () -> Build_log.copy ~src:(r :> Eio_unix.source) ~dst:log) in
      let cid = Os.pread ~process ~stderr:(w :> Eio.Flow.sink) ["docker"; "create"; "--"; base] in
      Eio.Promise.await_exn copy;
      String.trim cid
    )
  in
  Fun.protect
    (fun () -> fn cid)
    ~finally:(fun () -> Os.exec ~process ["docker"; "rm"; "--"; cid])


let fetch ~process ~log ~rootfs base =
    with_container ~process ~log base (fun cid ->
      Os.with_pipe_between_children @@ fun ~r ~w ->
      Os.exec ~process ~stdout:(w :> Eio.Flow.sink) ["docker"; "export"; "--"; cid];
      Os.sudo ~process ~stdin:(r :> Eio.Flow.source) ["tar"; "-C"; snd rootfs; "-xf"; "-"]
    );
    export_env ~process base