type unix_fd = {
  raw : Unix.file_descr;
  mutable needs_close : bool;
  }

let rec waitpid_non_intr pid =
  match Unix.waitpid [] pid with
  | v -> v
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

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
  (try Unix.close fd.raw with Unix.Unix_error _ -> ());
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then
    close fd

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

let dev_null_fd = lazy (Unix.openfile "/dev/null" [Unix.O_RDWR] 0)

let setup_fd = function
  | Some (`FD_copy fd) -> fd
  | Some `Dev_null -> Lazy.force dev_null_fd
  | None -> Unix.stdin  (* placeholder, won't be used if not set *)

(* stdin, stdout and stderr are copied to the child and then closed on the host.
   They are closed at most once, so duplicates are OK. *)
let default_exec ?timeout:(_:float option) ?cwd ?stdin ?stdout ?stderr ~pp argv =
  let stdin_r  = Option.map redirection stdin in
  let stdout_r = Option.map redirection stdout in
  let stderr_r = Option.map redirection stderr in
  let result =
    try
      let cmd = fst argv in
      let args = snd argv in
      let env = Unix.environment () in
      let stdin_fd = match stdin_r with
        | Some (`FD_copy fd) -> fd
        | Some `Dev_null -> Lazy.force dev_null_fd
        | None -> Unix.stdin
      in
      let stdout_fd = match stdout_r with
        | Some (`FD_copy fd) -> fd
        | Some `Dev_null -> Lazy.force dev_null_fd
        | None -> Unix.stdout
      in
      let stderr_fd = match stderr_r with
        | Some (`FD_copy fd) -> fd
        | Some `Dev_null -> Lazy.force dev_null_fd
        | None -> Unix.stderr
      in
      let prog = if cmd = "" then args.(0) else cmd in
      let cwd_args = match cwd with
        | None -> []
        | Some _ -> []  (* handled below *)
      in
      ignore cwd_args;
      (* Save/restore cwd if needed *)
      let old_cwd = match cwd with Some _ -> Some (Unix.getcwd ()) | None -> None in
      (match cwd with Some d -> Unix.chdir d | None -> ());
      let pid = Unix.create_process_env prog args env stdin_fd stdout_fd stderr_fd in
      (match old_cwd with Some d -> Unix.chdir d | None -> ());
      let _, status = waitpid_non_intr pid in
      match status with
      | Unix.WEXITED n -> Ok n
      | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
      | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x
    with e ->
      Fmt.error_msg "%t raised %s\n%s" pp (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  Option.iter close_redirection stdin;
  Option.iter close_redirection stdout;
  Option.iter close_redirection stderr;
  result

(* Similar to default_exec except using Unix.create_process in order to get the
   pid of the forked process. On macOS this allows for cleaner job cancellations *)
let open_process ?cwd ?env ?stdin ?stdout ?stderr ?pp:_ argv =
  Logs.info (fun f -> f "Fork exec %a" pp_cmd ("", argv));
  let stdin_fd = match stdin with
    | Some (`FD_move_safely fd) -> fd.raw
    | Some `Dev_null -> Lazy.force dev_null_fd
    | None -> Unix.stdin
  in
  let stdout_fd = match stdout with
    | Some (`FD_move_safely fd) -> fd.raw
    | Some `Dev_null -> Lazy.force dev_null_fd
    | None -> Unix.stdout
  in
  let stderr_fd = match stderr with
    | Some (`FD_move_safely fd) -> fd.raw
    | Some `Dev_null -> Lazy.force dev_null_fd
    | None -> Unix.stderr
  in
  let old_cwd = match cwd with Some _ -> Some (Unix.getcwd ()) | None -> None in
  (match cwd with Some d -> Unix.chdir d | None -> ());
  let env_arr = match env with Some e -> e | None -> Unix.environment () in
  let prog = List.hd argv in
  let args = Array.of_list argv in
  let pid = Unix.create_process_env prog args env_arr stdin_fd stdout_fd stderr_fd in
  (match old_cwd with Some d -> Unix.chdir d | None -> ());
  Option.iter close_redirection (Option.map (fun x -> (x : [`FD_move_safely of unix_fd | `Dev_null])) stdin);
  Option.iter close_redirection (Option.map (fun x -> (x : [`FD_move_safely of unix_fd | `Dev_null])) stdout);
  Option.iter close_redirection (Option.map (fun x -> (x : [`FD_move_safely of unix_fd | `Dev_null])) stderr);
  let wait_for_result () =
    let _, status = waitpid_non_intr pid in
    status
  in
  (pid, wait_for_result)

let process_result ~pp proc =
  let status = proc () in
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED n -> Fmt.error_msg "%t failed with exit status %a" pp pp_exit_status n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x

(* Overridden in unit-tests *)
let process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~pp ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  match !process_exec ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) with
  | Ok n when is_success n -> Ok ()
  | Ok n -> Fmt.error_msg "%t failed with exit status %a" pp pp_exit_status n
  | Error e -> Error (e : [`Msg of string] :> [> `Msg of string])

let exec ?timeout ?cwd ?stdin ?stdout ?stderr ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let pp f = pp_cmd f (cmd, argv) in
  match !process_exec ?timeout ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) with
  | Ok n when is_success n -> ()
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
  if len = 0 then ()
  else (
    let n = Unix.write fd buf ofs len in
    write_all fd buf (ofs + n) (len - n)
  )

let rec write_all_string fd buf ofs len =
  assert (len >= 0);
  if len = 0 then ()
  else (
    let n = Unix.write_substring fd buf ofs len in
    write_all_string fd buf (ofs + n) (len - n)
  )

let write_file ~path contents =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC] 0o666 in
  Fun.protect ~finally:(fun () -> (try Unix.close fd with Unix.Unix_error _ -> ())) @@ fun () ->
  write_all_string fd contents 0 (String.length contents)

let with_pipe_from_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let w = { raw = w; needs_close = true } in
  let r_fd = r in
  Fun.protect
    (fun () -> fn ~r:r_fd ~w)
    ~finally:(fun () ->
       ensure_closed_unix w;
       (try Unix.close r_fd with Unix.Unix_error _ -> ())
    )

let with_pipe_to_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w_fd = w in
  Fun.protect
    (fun () -> fn ~r ~w:w_fd)
    ~finally:(fun () ->
       ensure_closed_unix r;
       (try Unix.close w_fd with Unix.Unix_error _ -> ())
    )

let with_pipe_between_children fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
       ensure_closed_unix r;
       ensure_closed_unix w
    )

let pread ?timeout ?stderr argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child_result = ref (Ok ()) in
  let () = child_result := (try exec ?timeout ~stdout:(`FD_move_safely w) ?stderr argv; Ok () with Failure m -> Error (`Msg m)) in
  (* Read all data from the pipe *)
  let buf = Buffer.create 1024 in
  let tmp = Bytes.create 4096 in
  let rec read_all () =
    match Unix.read r tmp 0 (Bytes.length tmp) with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf tmp 0 n; read_all ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all ()
  in
  read_all ();
  (match !child_result with Ok () -> () | Error (`Msg m) -> failwith m);
  Buffer.contents buf

let pread_result ?cwd ?stdin ?stderr ~pp ?is_success ?cmd argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child = exec_result ?cwd ?stdin ~stdout:(`FD_move_safely w) ?stderr ~pp ?is_success ?cmd argv in
  let buf = Buffer.create 1024 in
  let tmp = Bytes.create 4096 in
  let rec read_all () =
    match Unix.read r tmp 0 (Bytes.length tmp) with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf tmp 0 n; read_all ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all ()
  in
  read_all ();
  let data = Buffer.contents buf in
  Result.map (fun () -> data) child

let pread_all ?stdin ~pp ?(cmd="") argv =
  with_pipe_from_child @@ fun ~r:r1 ~w:w1 ->
  with_pipe_from_child @@ fun ~r:r2 ~w:w2 ->
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let child =
    !process_exec ?stdin ~stdout:(`FD_move_safely w1) ~stderr:(`FD_move_safely w2) ~pp
      (cmd, Array.of_list argv)
  in
  let read_fd fd =
    let buf = Buffer.create 1024 in
    let tmp = Bytes.create 4096 in
    let rec read_all () =
      match Unix.read fd tmp 0 (Bytes.length tmp) with
      | 0 -> ()
      | n -> Buffer.add_subbytes buf tmp 0 n; read_all ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_all ()
    in
    read_all ();
    Buffer.contents buf
  in
  let stdout_data = read_fd r1 in
  let stderr_data = read_fd r2 in
  match child with
  | Ok i -> (i, stdout_data, stderr_data)
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
  match sudo_result ~pp:(pp "RM") ["rm"; "-r"; directory ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to remove %s because %s" directory m)

let mv ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ MV ]" in
  match sudo_result ~pp:(pp "MV") ["mv"; src; dst ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to move %s to %s because %s" src dst m)

let cp ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ CP ]" in
  match sudo_result ~pp:(pp "CP") ["cp"; "-pRduT"; "--reflink=auto"; src; dst ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to copy from %s to %s because %s" src dst m)

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
