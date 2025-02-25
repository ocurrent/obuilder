open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

type unix_fd = {
  raw : Unix.file_descr;
  mutable needs_close : bool;
  }

let stdout = {
  raw = Unix.stdout;
  needs_close = false;
  }

let stderr = {
  raw = Unix.stderr;
  needs_close = false;
}

let close fd =
  assert (fd.needs_close);
  Unix.close fd.raw;
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then
    close fd

let ensure_closed_lwt fd =
  if Lwt_unix.state fd = Lwt_unix.Closed then Lwt.return_unit
  else Lwt_unix.close fd

let pp_cmd f (cmd, argv) =
  let argv = if cmd = "" then argv else cmd :: argv in
  Fmt.hbox Fmt.(list ~sep:sp (quote string)) f argv

let pp_exit_status f n =
  if Sys.win32 && n < 0 then
    Fmt.pf f "0x%08lx" (Int32.of_int n)
  else
    Fmt.int f n

let redirection = function
  | `FD_move_safely x -> `FD_copy x.raw
  | `Dev_null -> `Dev_null

let close_redirection (x : [`FD_move_safely of unix_fd | `Dev_null]) =
  match x with
  | `FD_move_safely x -> ensure_closed_unix x
  | `Dev_null -> ()

(* stdin, stdout and stderr are copied to the child and then closed on the host.
   They are closed at most once, so duplicates are OK. *)
let default_exec ?timeout ?cwd ?stdin ?stdout ?stderr ~pp argv =
  let proc =
    let stdin  = Option.map redirection stdin in
    let stdout = Option.map redirection stdout in
    let stderr = Option.map redirection stderr in
    try Lwt_result.ok (Lwt_process.exec ?timeout ?cwd ?stdin ?stdout ?stderr argv)
    with e -> Lwt_result.fail e
  in
  Option.iter close_redirection stdin;
  Option.iter close_redirection stdout;
  Option.iter close_redirection stderr;
  proc >|= fun proc ->
  Result.fold ~ok:(function
      | Unix.WEXITED n -> Ok n
      | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
      | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x)
    ~error:(fun e ->
        Fmt.error_msg "%t raised %s\n%s" pp (Printexc.to_string e) (Printexc.get_backtrace ())) proc

(* Similar to default_exec except using open_process_none in order to get the
   pid of the forked process. On macOS this allows for cleaner job cancellations *)
let open_process ?cwd ?env ?stdin ?stdout ?stderr ?pp:_ argv =
  Logs.info (fun f -> f "Fork exec %a" pp_cmd ("", argv));
  let proc =
    let stdin  = Option.map redirection stdin in
    let stdout = Option.map redirection stdout in
    let stderr = Option.map redirection stderr in
    let process = Lwt_process.open_process_none ?cwd ?env ?stdin ?stdout ?stderr ("", (Array.of_list argv)) in
  (process#pid, process#status)
  in
    Option.iter close_redirection stdin;
    Option.iter close_redirection stdout;
    Option.iter close_redirection stderr;
    proc

let process_result ~pp proc =
  proc >|= (function
  | Unix.WEXITED n -> Ok n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x)
  >>= function
  | Ok 0 -> Lwt_result.return ()
  | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %a" pp pp_exit_status n
  | Error e -> Lwt_result.fail (e : [`Msg of string] :> [> `Msg of string])

(* Overridden in unit-tests *)
let lwt_process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~pp ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) >>= function
  | Ok n when is_success n -> Lwt_result.ok Lwt.return_unit
  | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %a" pp pp_exit_status n
  | Error e -> Lwt_result.fail (e : [`Msg of string] :> [> `Msg of string])

let exec ?timeout ?cwd ?stdin ?stdout ?stderr ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let pp f = pp_cmd f (cmd, argv) in
  !lwt_process_exec ?timeout ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) >>= function
  | Ok n when is_success n -> Lwt.return_unit
  | Ok n -> Fmt.failwith "%t failed with exit status %a" pp pp_exit_status n
  | Error (`Msg m) -> failwith m

let running_as_root = not (Sys.unix) || Unix.getuid () = 0

let sudo ?stdin args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec ?stdin args

let sudo_result ?cwd ?stdin ?stdout ?stderr ?is_success ~pp args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec_result ?cwd ?stdin ?stdout ?stderr ?is_success ~pp args

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)
  )

let rec write_all_string fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write_string fd buf ofs len >>= fun n ->
    write_all_string fd buf (ofs + n) (len - n)
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

let pread ?timeout ?stderr argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child = exec ?timeout ~stdout:(`FD_move_safely w) ?stderr argv in
  let r = Lwt_io.(of_fd ~mode:input) r in
  Lwt.finalize
    (fun () -> Lwt_io.read r)
    (fun () -> Lwt_io.close r)
  >>= fun data -> child >|= fun () -> data

let pread_result ?cwd ?stdin ?stderr ~pp ?is_success ?cmd argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child = exec_result ?cwd ?stdin ~stdout:(`FD_move_safely w) ?stderr ~pp ?is_success ?cmd argv in
  let r = Lwt_io.(of_fd ~mode:input) r in
  Lwt.finalize
    (fun () -> Lwt_io.read r)
    (fun () -> Lwt_io.close r)
  >>= fun data -> child >|= fun r -> Result.map (fun () -> data) r

let pread_all ?stdin ~pp ?(cmd="") argv =
  with_pipe_from_child @@ fun ~r:r1 ~w:w1 ->
  with_pipe_from_child @@ fun ~r:r2 ~w:w2 ->
  let child =
    Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
    !lwt_process_exec ?stdin ~stdout:(`FD_move_safely w1) ~stderr:(`FD_move_safely w2) ~pp
      (cmd, Array.of_list argv)
  in
  let r1 = Lwt_io.(of_fd ~mode:input) r1 in
  let r2 = Lwt_io.(of_fd ~mode:input) r2 in
  Lwt.finalize
    (fun () -> Lwt.both (Lwt_io.read r1) (Lwt_io.read r2))
    (fun () -> Lwt.both (Lwt_io.close r1) (Lwt_io.close r2) >>= fun _ -> Lwt.return_unit)
  >>= fun (stdin, stdout) ->
  child >>= function
  | Ok i -> Lwt.return (i, stdin, stdout)
  | Error (`Msg m) -> failwith m

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let ensure_dir ?(mode=0o777) path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> Unix.mkdir path mode

let read_link x =
  match Unix.readlink x with
  | s -> Some s
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> None

let rm ~directory =
  let pp _ ppf = Fmt.pf ppf "[ RM ]" in
  sudo_result ~pp:(pp "RM") ["rm"; "-r"; directory ] >>= fun t ->
  match t with
  | Ok () -> Lwt.return_unit
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to remove %s because %s" directory m);
    Lwt.return_unit

let mv ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ MV ]" in
  sudo_result ~pp:(pp "MV") ["mv"; src; dst ] >>= fun t ->
  match t with
  | Ok () -> Lwt.return_unit
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to move %s to %s because %s" src dst m);
    Lwt.return_unit

let cp ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ CP ]" in
  sudo_result ~pp:(pp "CP") ["cp"; "-pRduT"; "--reflink=auto"; src; dst ] >>= fun t ->
  match t with
  | Ok () -> Lwt.return_unit
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to copy from %s to %s because %s" src dst m);
    Lwt.return_unit

let normalise_path root_dir =
  if Sys.win32 then
    let vol, _ = Fpath.(v root_dir |> split_volume) in
    vol ^ "\\"
  else
    root_dir

let free_space_percent root_dir =
  let vfs = ExtUnix.All.statvfs (normalise_path root_dir) in
  let used = Int64.sub vfs.f_blocks vfs.f_bfree in
  100. -. 100. *. (Int64.to_float used) /. Int64.(to_float (add used vfs.f_bavail))

let read_lines name process =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop ((process s) :: acc)
    | None -> close_in ic; acc in
  loop []
