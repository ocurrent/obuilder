open Lwt.Infix

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.(Dump.list Dump.string)

let exec ?cwd ?stdin ?stdout ?stderr argv =
  Lwt_process.exec ?cwd ?stdin ?stdout ?stderr ("", Array.of_list argv) >|= function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Fmt.failwith "%a failed with exit status %d" pp_cmd argv n
  | Unix.WSIGNALED x -> Fmt.failwith "%a failed with signal %d" pp_cmd argv x
  | Unix.WSTOPPED x -> Fmt.failwith "%a stopped with signal %a" pp_cmd argv pp_signal x

let with_open_out path fn =
  Lwt_unix.openfile path Unix.[O_RDWR; O_CREAT] 0o644 >>= fun fd ->
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

let tee ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n ->
      output stdout buf 0 n;
      flush stdout;
      write_all dst buf 0 n >>= aux
  in
  aux ()
