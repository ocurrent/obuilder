open Lwt.Infix

module Os = Obuilder.Os

let ( / ) = Filename.concat

let strf = Printf.sprintf

let unix_path path =
  if Sys.win32 then
    Lwt_process.pread ("", [| "cygpath"; "-u"; path|]) >|= fun str -> String.trim str
  else
    Lwt.return path

let next_container_id = ref 0

let base_tar =
  let mydir = Sys.getcwd () in
  Lwt_main.run begin
    let base_tar = mydir / "base.tar" in
    Lwt_io.(with_file ~mode:input) base_tar Lwt_io.read
  end
  |> Bytes.of_string

let with_fd x f =
  match x with
  | `FD_move_safely fd ->
    let copy = Unix.dup ~cloexec:true fd.Os.raw in
    Os.close fd;
    Lwt.finalize
      (fun () -> f copy)
      (fun () -> Unix.close copy; Lwt.return_unit)
  | _ -> failwith "Unsupported mock FD redirection"

let docker_create ?stdout base =
  with_fd (Option.get stdout) @@ fun stdout ->
  let id = strf "%s-%d\n" base !next_container_id in
  incr next_container_id;
  let rec aux i =
    let len = String.length id - i in
    if len = 0 then Lwt_result.return 0
    else (
      let sent = Unix.single_write_substring stdout id i len in
      aux (i + sent)
    )
  in
  aux 0

let docker_export ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  let stdout = Lwt_unix.of_unix_file_descr stdout in
  Os.write_all stdout base_tar 0 (Bytes.length base_tar) >|= fun () ->
  Ok 0

let docker_inspect ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  let stdout = Lwt_unix.of_unix_file_descr stdout in
  let msg = Bytes.of_string "PATH=/usr/bin:/usr/local/bin" in
  Os.write_all stdout msg 0 (Bytes.length msg) >|= fun () ->
  Ok 0

let exec_docker ?stdout = function
  | ["create"; "--"; base] -> docker_create ?stdout base
  | ["export"; "--"; id] -> docker_export ?stdout id
  | ["image"; "inspect"; "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|}; "--"; base] -> docker_inspect ?stdout base
  | ["rm"; "--force"; "--"; id] -> Fmt.pr "docker rm --force %S@." id; Lwt_result.return 0
  | x -> Fmt.failwith "Unknown mock docker command %a" Fmt.(Dump.list string) x

let mkdir = function
  | ["-m"; "755"; "--"; path] -> Unix.mkdir path 0o755; Lwt_result.return 0
  | x -> Fmt.failwith "Unexpected mkdir %a" Fmt.(Dump.list string) x

let closing redir fn =
  Lwt.finalize fn
    (fun () ->
       begin match redir with
         | Some (`FD_move_safely fd) -> Os.ensure_closed_unix fd
         | _ -> ()
       end;
       Lwt.return_unit
    )

let exec ?timeout ?cwd ?stdin ?stdout ?stderr ~pp cmd =
  ignore timeout;
  closing stdin @@ fun () ->
  closing stdout @@ fun () ->
  closing stderr @@ fun () ->
  match cmd with
  | ("", argv) ->
    Fmt.pr "exec: %a@." Fmt.(Dump.array string) argv;
    begin match Array.to_list argv with
      | "docker" :: args -> exec_docker ?stdout args
      | "sudo" :: "--" :: ("tar" :: _ as tar) when not Os.running_as_root ->
        Os.default_exec ?cwd ?stdin ?stdout ~pp ("", Array.of_list tar)
      | "tar" :: "-C" :: path :: opts when Os.running_as_root ->
        unix_path path >>= fun path ->
        let tar = (if Sys.win32 then "C:\\cygwin64\\bin\\tar.exe" else "tar") :: "-C" :: path :: opts in
        Os.default_exec ?cwd ?stdin ?stdout ~pp ("", Array.of_list tar)
      | "mkdir" :: args when Os.running_as_root -> mkdir args
      | "sudo" :: "--" :: "mkdir" :: args when not Os.running_as_root -> mkdir args
      | x -> Fmt.failwith "Unknown mock command %a" Fmt.(Dump.list string) x
    end
  | (x, _) -> Fmt.failwith "Unexpected absolute path: %S" x
