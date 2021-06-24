open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

type unix_fd = {
  raw : Unix.file_descr;
  mutable needs_close : bool;
}

let close fd =
  assert (fd.needs_close);
  Unix.close fd.raw;
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then close fd

let ensure_closed_lwt fd =
  if Lwt_unix.state fd = Lwt_unix.Closed then Lwt.return_unit
  else Lwt_unix.close fd

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.box Fmt.(list ~sep:sp (quote string))

let redirection = function
  | `FD_move_safely x -> `FD_copy x.raw
  | `Dev_null -> `Dev_null

let close_redirection (x : [`FD_move_safely of unix_fd | `Dev_null]) =
  match x with
  | `FD_move_safely x -> ensure_closed_unix x
  | `Dev_null -> ()

(* stdin, stdout and stderr are copied to the child and then closed on the host.
   They are closed at most once, so duplicates are OK. *)
let default_exec ?cwd ?stdin ?stdout ?stderr ~pp argv =
  let proc =
    let stdin  = Option.map redirection stdin in
    let stdout = Option.map redirection stdout in
    let stderr = Option.map redirection stderr in
    try Lwt_result.ok (Lwt_process.exec ?cwd ?stdin ?stdout ?stderr argv)
    with e -> Lwt_result.fail e
  in
  Option.iter close_redirection stdin;
  Option.iter close_redirection stdout;
  Option.iter close_redirection stderr;
  proc >|= fun proc ->
  Result.fold ~ok:(function
      | Unix.WEXITED n -> Ok n
      | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %d" pp x
      | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp pp_signal x)
    ~error:(fun e ->
        Fmt.error_msg "%t raised %s\n%s" pp (Printexc.to_string e) (Printexc.get_backtrace ())) proc

(* Similar to default_exec except using open_process_none in order to get the
   pid of the forked process. On macOS this allows for cleaner job cancellations *)
let open_process ?cwd ?stdin ?stdout ?stderr ?pp:_ argv =
  Logs.info (fun f -> f "Fork exec %a" pp_cmd argv);
  let proc =
    let stdin  = Option.map redirection stdin in
    let stdout = Option.map redirection stdout in
    let stderr = Option.map redirection stderr in
    let process = Lwt_process.open_process_none ?cwd ?stdin ?stdout ?stderr ("", (Array.of_list argv)) in
  (process#pid, process#status)
  in
    Option.iter close_redirection stdin;
    Option.iter close_redirection stdout;
    Option.iter close_redirection stderr;
    proc

let process_result ~pp proc =
  proc >|= (function
  | Unix.WEXITED n -> Ok n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %d" pp x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp pp_signal x)
  >>= function
  | Ok 0 -> Lwt_result.return ()
  | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %d" pp n
  | Error e -> Lwt_result.fail (e : [`Msg of string] :> [> `Msg of string])

(* Overridden in unit-tests *)
let lwt_process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~pp ?(is_success=((=) 0)) argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>= function
  | Ok n when is_success n -> Lwt_result.return ()
  | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %d" pp n
  | Error e -> Lwt_result.fail (e : [`Msg of string] :> [> `Msg of string])

let exec ?cwd ?stdin ?stdout ?stderr ?(is_success=((=) 0)) argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  let pp f = pp_cmd f argv in
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>= function
  | Ok n when is_success n -> Lwt.return_unit
  | Ok n -> Lwt.fail_with (Fmt.str "%t failed with exit status %d" pp n)
  | Error (`Msg m) -> Lwt.fail (Failure m)

let running_as_root = not (Sys.unix) || Unix.getuid () = 0

let sudo ?stdin args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec ?stdin args

let sudo_result ?cwd ?stdin ?stdout ?stderr ~pp args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec_result ?cwd ?stdin ?stdout ?stderr ~pp args

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)
  )

let write_file ~path contents =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in
  Lwt_io.(with_file ~mode:output ~flags) path @@ fun ch ->
  Lwt_io.write ch contents

let with_pipe_from_child fn =
  let r, w = Lwt_unix.pipe_in ~cloexec:true () in
  let w = { raw = w; needs_close = true } in
  Lwt.finalize
    (fun () -> fn ~r ~w)
    (fun () ->
       ensure_closed_unix w;
       ensure_closed_lwt r
    )

let with_pipe_to_child fn =
  let r, w = Lwt_unix.pipe_out ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  Lwt.finalize
    (fun () -> fn ~r ~w)
    (fun () ->
       ensure_closed_unix r;
       ensure_closed_lwt w
    )

let with_pipe_between_children fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Lwt.finalize
    (fun () -> fn ~r ~w)
    (fun () ->
       ensure_closed_unix r;
       ensure_closed_unix w;
       Lwt.return_unit
    )

let pread ?stderr argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child = exec ~stdout:(`FD_move_safely w) ?stderr argv in
  let r = Lwt_io.(of_fd ~mode:input) r in
  Lwt.finalize
    (fun () -> Lwt_io.read r)
    (fun () -> Lwt_io.close r)
  >>= fun data ->
  child >>= fun () ->
  Lwt.return data

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let ensure_dir path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> Unix.mkdir path 0o777
