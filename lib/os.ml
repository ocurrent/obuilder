open Lwt.Infix
open Sexplib.Std

type env = (string * string) list [@@deriving sexp]

let ( >>!= ) = Lwt_result.bind

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.box Fmt.(list ~sep:sp (quote string))

let default_exec ?cwd ?stdin ?stdout ?stderr ~pp argv =
  Lwt_process.exec ?cwd ?stdin ?stdout ?stderr argv >|= function
  | Unix.WEXITED n -> Ok n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %d" pp x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp pp_signal x

(* Overridden in unit-tests *)
let lwt_process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~pp argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>!= function
  | 0 -> Lwt_result.return ()
  | n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %d" pp n

let exec ?cwd ?stdin ?stdout ?stderr argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  let pp f = pp_cmd f argv in
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>= function
  | Ok 0 -> Lwt.return_unit
  | Ok n -> Lwt.fail_with (Fmt.strf "%t failed with exit status %d" pp n)
  | Error (`Msg m) -> Lwt.fail (Failure m)

let with_open_out path fn =
  Lwt_unix.openfile path Unix.[O_RDWR; O_CREAT; O_EXCL] 0o666 >>= fun fd ->
  Lwt.finalize
    (fun () -> fn fd)
    (fun () -> Lwt_unix.close fd)

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)
  )

let write_file ~path contents =
  Lwt_io.(with_file ~mode:output) path @@ fun ch ->
  Lwt_io.write ch contents

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

let with_pipe_from_child fn =
  let r, w = Lwt_unix.pipe_in () in
  let w = { raw = w; needs_close = true } in
  Lwt.finalize
    (fun () ->
       Lwt_unix.set_close_on_exec r;
       fn ~r ~w
    )
    (fun () ->
       ensure_closed_unix w;
       ensure_closed_lwt r
    )

let with_pipe_to_child fn =
  let r, w = Lwt_unix.pipe_out () in
  let r = { raw = r; needs_close = true } in
  Lwt.finalize
    (fun () ->
       Lwt_unix.set_close_on_exec w;
       fn ~r ~w
    )
    (fun () ->
       ensure_closed_unix r;
       ensure_closed_lwt w
    )

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let ensure_dir path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> Unix.mkdir path 0o777
