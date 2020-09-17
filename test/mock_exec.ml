open Lwt.Infix

module Os = Obuilder.Os

let ( / ) = Filename.concat

let strf = Printf.sprintf

let next_container_id = ref 0

let base_tar =
  let mydir = Sys.getcwd () in
  Lwt_main.run (Lwt_io.(with_file ~mode:input) (mydir / "base.tar") Lwt_io.read)
  |> Bytes.of_string

let with_fd x f =
  match x with
  | `FD_copy fd ->
    let copy = Unix.dup ~cloexec:true fd in
    Lwt.finalize
      (fun () -> f copy)
      (fun () -> Unix.close copy; Lwt.return ())
  | _ -> failwith "Unsupported mock FD redirection"

let docker_create ?stdout base =
  with_fd (Option.get stdout) @@ fun stdout ->
  let id = strf "%s-%d\n" base !next_container_id in
  incr next_container_id;
  let rec aux i =
    let len = String.length id - i in
    if len = 0 then Lwt.return (Unix.WEXITED 0)
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
  Unix.WEXITED 0

let exec_docker ?stdout = function
  | ["create"; "--"; base] -> docker_create ?stdout base
  | ["export"; "--"; id] -> docker_export ?stdout id
  | ["rm"; "--"; id] -> Fmt.pr "docker rm %S@." id; Lwt.return (Unix.WEXITED 0)
  | x -> Fmt.failwith "Unknown mock docker command %a" Fmt.(Dump.list string) x

let exec ?timeout:_ ?env:_ ?cwd ?stdin ?stdout ?stderr:_ = function
  | ("", argv) ->
    Fmt.pr "exec: %a@." Fmt.(Dump.array string) argv;
    begin match Array.to_list argv with
      | "docker" :: args -> exec_docker ?stdout args
      | "sudo" :: ("tar" :: _ as tar) -> Lwt_process.exec ?cwd ?stdin ?stdout ("", Array.of_list tar)
      | x -> Fmt.failwith "Unknown mock command %a" Fmt.(Dump.list string) x
    end
  | (x, _) -> Fmt.failwith "Unexpected absolute path: %S" x
