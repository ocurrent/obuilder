let invoke_fetcher base destdir =
  Os.with_pipe_between_children @@ fun ~r ~w ->
    let stdin = `FD_move_safely r in
    let stdout = `FD_move_safely w in
    let stderr = stdout in
    Os.exec ~stdout ~stderr ["fetch"; "-q" ; "-o" ; "-" ; base ];
    Os.sudo ~stdin [ "tar" ; "-C"; destdir ; "-xzpf"; "-" ]

let fetch ~log:_ ~root:_ ~rootfs base =
  (try
     invoke_fetcher base rootfs;
     []
   with
   | Sys_error s ->
     Fmt.failwith "Archive fetcher encountered a system error: %s" s)
