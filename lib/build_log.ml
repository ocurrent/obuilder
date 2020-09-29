open Lwt.Infix

let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of Lwt_unix.file_descr * unit Lwt_condition.t  (* Fires after writing more data. *)
    | `Readonly of string
    | `Empty
    | `Finished
  ];
  mutable len : int;
}

let with_dup fd fn =
  let fd = Lwt_unix.dup fd in
  Lwt_unix.set_close_on_exec fd;
  Lwt.finalize
    (fun () -> fn fd)
    (fun () -> Lwt_unix.close fd)

let catch_cancel fn =
  Lwt.catch fn
    (function
      | Lwt.Canceled -> Lwt_result.fail `Cancelled
      | ex -> Lwt.fail ex
    )

let tail ?switch t dst =
  match t.state with
  | `Finished -> invalid_arg "tail: log is finished!"
  | `Readonly path ->
    Lwt_io.(with_file ~mode:input) path @@ fun ch ->
    let buf = Bytes.create max_chunk_size in
    let rec aux () =
      Lwt_io.read_into ch buf 0 max_chunk_size >>= function
      | 0 -> Lwt_result.return ()
      | n -> dst (Bytes.sub_string buf 0 n); aux ()
    in
    catch_cancel @@ fun () ->
    let th = aux () in
    Lwt_switch.add_hook_or_exec switch (fun () -> Lwt.cancel th; Lwt.return_unit) >>= fun () ->
    th
  | `Empty -> Lwt_result.return ()
  | `Open (fd, cond) ->
    (* Dup [fd], which can still work after [fd] is closed. *)
    with_dup fd @@ fun fd ->
    let buf = Bytes.create max_chunk_size in
    let rec aux i =
      match switch with
      | Some sw when not (Lwt_switch.is_on sw) -> Lwt_result.fail `Cancelled
      | _ ->
        let avail = min (t.len - i) max_chunk_size in
        if avail > 0 then (
          Lwt_unix.pread fd ~file_offset:i buf 0 avail >>= fun n ->
          dst (Bytes.sub_string buf 0 n);
          aux (i + avail)
        ) else (
          match t.state with
          | `Open _ -> Lwt_condition.wait cond >>= fun () -> aux i
          | _ -> Lwt_result.return ()
        )
    in
    catch_cancel @@ fun () ->
    let th = aux 0 in
    Lwt_switch.add_hook_or_exec switch (fun () -> Lwt.cancel th; Lwt.return_unit) >>= fun () ->
    th

let create path =
  Lwt_unix.openfile path Lwt_unix.[O_CREAT; O_TRUNC; O_RDWR; O_CLOEXEC] 0o666 >|= fun fd ->
  let cond = Lwt_condition.create () in
  {
    state = `Open (fd, cond);
    len = 0;
  }

let finish t =
  match t.state with
  | `Finished -> invalid_arg "Log is already finished!"
  | `Open (fd, cond) ->
    t.state <- `Finished;
    Lwt_unix.close fd >|= fun () ->
    Lwt_condition.broadcast cond ()
  | `Readonly _ ->
    t.state <- `Finished;
    Lwt.return_unit
  | `Empty ->
    Lwt.return_unit (* Empty can be reused *)

let write t data =
  match t.state with
  | `Finished -> invalid_arg "write: log is finished!"
  | `Readonly _ | `Empty -> invalid_arg "Log is read-only!"
  | `Open (fd, cond) ->
    let len = String.length data in
    Os.write_all fd (Bytes.of_string data) 0 len >>= fun () ->
    t.len <- t.len + len;
    Lwt_condition.broadcast cond ();
    Lwt.return_unit

let of_saved path =
  Lwt_unix.lstat path >|= fun stat ->
  {
    state = `Readonly path;
    len = stat.st_size;
  }

let printf t fmt =
  Fmt.kstrf (write t) fmt

let empty = {
  state = `Empty;
  len = 0;
}
