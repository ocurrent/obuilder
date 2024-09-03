open Lwt.Infix

let invoke_fetcher base destdir =
  Os.with_pipe_between_children @@ fun ~r ~w ->
    let stdin = `FD_move_safely r in
    let stdout = `FD_move_safely w in
    let stderr = stdout in
    let fetcher =
      Os.exec ~stdout ~stderr ["fetch"; "-q" ; "-o" ; "-" ; base ]
    in
    let extracter =
      Os.sudo ~stdin [ "tar" ; "-C"; destdir ; "-xzpf"; "-" ]
    in
    fetcher >>= fun () ->
    extracter

let fetch ~log ~rootfs base =
  let _ = log in
  Lwt.catch
    (fun () ->
     invoke_fetcher base rootfs >>= fun () ->
     let env = [] in
     Lwt.return env)
    (function
     | Sys_error s ->
       Fmt.failwith "Archive fetcher encountered a system error: %s" s
     | ex -> Lwt.reraise ex)
