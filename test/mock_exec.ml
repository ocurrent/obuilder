module Os = Obuilder.Os

let ( / ) = Filename.concat

let strf = Printf.sprintf

let next_container_id = ref 0

let base_tar =
  let mydir = Sys.getcwd () in
  Lwt_main.run (Lwt_io.(with_file ~mode:input) (mydir / "base.tar") Lwt_io.read)
  |> Cstruct.of_string

let with_fd ~sw (fd : Eio.Flow.sink) f =
  let copy = Eio_unix.dup ~sw (fd :> Eio.Generic.t) in
  Option.iter Unix.close (Eio_unix.FD.peek_opt fd);
  Fun.protect
    (fun () -> f copy)
    ~finally:(fun () -> Eio.Flow.close copy)

let docker_create ?stdout base =
  with_fd (Option.get stdout) @@ fun stdout ->
  let id = strf "%s-%d\n" base !next_container_id in
  incr next_container_id;
  let rec aux i =
    let len = String.length id - i in
    if len = 0 then Ok 0
    else (
      let sent = Unix.single_write_substring (stdout#unix_fd `Peek) id i len in
      aux (i + sent)
    )
  in
  aux 0

let docker_export ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  Os.write_all stdout base_tar 0 (Cstruct.length base_tar);
  Ok 0

let docker_inspect ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  let msg = Cstruct.of_string "PATH=/usr/bin:/usr/local/bin" in
  Os.write_all stdout msg 0 (Cstruct.length msg);
  Ok 0

let exec_docker ~sw ?stdout = function
  | ["create"; "--"; base] -> docker_create ~sw ?stdout base
  | ["export"; "--"; id] -> docker_export ~sw ?stdout id
  | ["image"; "inspect"; "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|}; "--"; base] -> docker_inspect ~sw ?stdout base
  | ["rm"; "--"; id] -> Fmt.pr "docker rm %S@." id; Ok 0
  | x -> Fmt.failwith "Unknown mock docker command %a" Fmt.(Dump.list string) x

let mkdir = function
  | ["--mode=755"; "--"; path] -> Unix.mkdir path 0o755; Ok 0
  | x -> Fmt.failwith "Unexpected mkdir %a" Fmt.(Dump.list string) x

let closing redir fn =
  Fun.protect fn
    ~finally:(fun () -> match redir with Some fd -> Os.ensure_closed_unix fd | _ -> ())

let exec ?cwd ?stdin ?stdout ?stderr ~sw ~process ~pp cmd =
  closing stdin @@ fun () ->
  closing stdout @@ fun () ->
  closing stderr @@ fun () ->
  match cmd with
  | ("", argv) ->
    Fmt.pr "exec: %a@." Fmt.(Dump.list string) argv;
    begin match argv with
      | "docker" :: args -> exec_docker ~sw ?stdout args
      | "sudo" :: ("tar" :: _ as tar) ->
        Os.default_exec ~sw ~process ?stdin ?stdout ?cwd ~pp ("tar", tar)
      | "sudo" :: "mkdir" :: args
      | "mkdir" :: args -> mkdir args
      | x -> Fmt.failwith "Unknown mock command %a" Fmt.(Dump.list string) x
    end
  | (x, _) -> Fmt.failwith "Unexpected absolute path: %S" x
