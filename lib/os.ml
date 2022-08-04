open Eio

let _close fd =
  match Eio_unix.FD.take_opt fd with
  | Some fd -> Unix.close fd
  | None -> assert false

let ensure_closed_unix _fd = ()

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.box Fmt.(list ~sep:sp (quote string))

let redirection = function
  | `FD_move_safely x -> `FD_copy x
  | `Dev_null -> `Dev_null

let close_redirection flow = ensure_closed_unix flow

(* stdin, stdout and stderr are copied to the child and then closed on the host.
   They are closed at most once, so duplicates are OK. *)
let default_exec ?cwd ?stdin ?stdout ?stderr ~sw ~(process:Process.t) ~pp (v, argv) =
  let proc =
    (* let _stdin  = Option.map redirection stdin in
    let _stdout = Option.map redirection stdout in
    let _stderr = Option.map redirection stderr in *)
    Process.spawn ?cwd ?stdin ?stdout ?stderr ~sw process v argv
  in
  (* Option.iter close_redirection stdin;
  Option.iter close_redirection stdout;
  Option.iter close_redirection stderr; *)
  match Process.status proc with
  | Exited n -> Ok n
  | Signaled x -> Fmt.error_msg "%t failed with signal %d" pp x
  | Stopped x -> Fmt.error_msg "%t stopped with signal %a" pp pp_signal x

(* Overridden in unit-tests *)
let eio_process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~sw ~process ~pp argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  match !eio_process_exec ?cwd ?stdin ?stdout ?stderr ~sw ~process ~pp ("", argv) with
  | Ok 0 -> Ok ()
  | Ok n -> Fmt.error_msg "%t failed with exit status %d" pp n
  | Error e -> Error (e : [`Msg of string] :> [> `Msg of string])

let exec ?cwd ?stdin ?stdout ?stderr ~sw ~process argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  let pp f = pp_cmd f argv in
  match !eio_process_exec ?cwd ?stdin ?stdout ?stderr ~sw ~process ~pp ("", argv) with
  | Ok 0 -> ()
  | Ok n -> failwith (Fmt.str "%t failed with exit status %d" pp n)
  | Error (`Msg m) -> raise (Failure m)

let running_as_root = not (Sys.unix) || Unix.getuid () = 0

let sudo ?stdin args =
  let args = if running_as_root then args else "sudo" :: args in
  exec ?stdin args

let sudo_result ?cwd ?stdin ?stdout ?stderr ~pp args =
  let args = if running_as_root then args else "sudo" :: args in
  exec_result ?cwd ?stdin ?stdout ?stderr ~pp args

let write_all fd buf off len =
  assert (len >= 0);
  if len = 0 then ()
  else (
    Flow.copy (Flow.cstruct_source [ Cstruct.sub buf off len ]) fd
  )

let write_file ~dir ~path contents =
  (* let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in *)
  Dir.save ~create:(`Or_truncate 0o600) dir path contents

let with_pipe_from_child ~sw fn =
  let r, w = Eio_unix.pipe sw in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
       ensure_closed_unix w;
       ensure_closed_unix r
    )

let with_pipe_to_child ~sw fn =
  let r, w = Eio_unix.pipe sw in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
       ensure_closed_unix r;
       ensure_closed_unix w
    )

let with_pipe_between_children ~sw fn =
  let r, w = Eio_unix.pipe sw in
  Fun.protect
    (fun () -> fn ~r ~w)
    ~finally:(fun () ->
       ensure_closed_unix r;
       ensure_closed_unix w
    )

let read_all ?(size=512) flow =
  let rec aux acc =
    try 
      let buf = Cstruct.create size in
      let i = Flow.read flow buf in
      aux (Cstruct.sub buf 0 i :: acc)
    with
      | End_of_file -> List.rev acc |> Cstruct.concat |> Cstruct.to_string
  in
    aux []

let pread ?stderr ~sw ~process argv =
  with_pipe_from_child ~sw @@ fun ~r ~w ->
  let () = exec ~sw ~process ~stdout:(w :> Flow.sink) ?stderr argv in
  let data = 
    Fun.protect
      (fun () -> read_all ~size:64000 r)
      ~finally:(fun () -> ())
  in
  data

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let ensure_dir path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> Unix.mkdir path 0o777
