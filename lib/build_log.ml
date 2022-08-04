open Eio

let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of <Flow.source; Flow.sink; Flow.close> * Condition.t  (* Fires after writing more data. *)
    | `Readonly of string
    | `Empty
    | `Finished
  ];
  dir : Dir.t;
  mutable len : int;
}

let with_dup ~sw (fd : 'a) fn =
  let copy = Eio_unix.dup ~sw fd in 
  fn copy
  (* ~finally:(fun () -> Flow.close copy) : Closed by switch? *)

let catch_cancel fn =
  try fn () with
      | Lwt.Canceled -> Error `Cancelled
      | ex -> raise ex

let tail ?switch t dst =
  match t.state with
  | `Finished -> invalid_arg "tail: log is finished!"
  | `Readonly path ->
    (* let flags = [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in *)
    Dir.with_open_in t.dir path @@ fun ch ->
    (* Lwt_io.(with_file ~mode:input ~flags) path @@ fun ch -> *)
    let buf = Cstruct.create max_chunk_size in
    let th, cancel =
      let th, set_th = Promise.create () in
      Switch.run @@ fun sw ->
      let rec aux () =
        try 
          match Flow.read ch (Cstruct.sub buf 0 max_chunk_size) with
          | 0 -> Promise.resolve set_th (Ok ())
          | n -> dst (Cstruct.to_string buf ~off:0 ~len:n); aux ()
        with End_of_file -> Promise.resolve set_th (Ok ())
      in
      Fiber.fork ~sw aux;
      th, fun () -> Switch.fail sw (Failure "cancelled")
    in
    catch_cancel @@ fun () ->
    (* Option.iter (fun sw -> Switch.on_release sw cancel) switch; *)
    (* Lwt_eio.Promise.await_lwt @@
    Lwt_switch.add_hook_or_exec switch (fun () -> Lwt.cancel th; Lwt.return_unit); *)
    Promise.await th
  | `Empty -> Ok ()
  | `Open (fd, cond) ->
    (* Dup [fd], which can still work after [fd] is closed. *)
    Switch.run @@ fun sw ->
    with_dup ~sw fd @@ fun fd ->
    let buf = Cstruct.create max_chunk_size in
    let rec aux i =
      (* match switch with
      | Some sw -> (
        try Ok (Switch.check sw) with Cancel.Cancelled _ -> Error `Cancelled
      )
      | _ -> *)
        let avail = min (t.len - i) max_chunk_size in
        if avail > 0 then (
          let n = Flow.read fd buf in
          dst (Cstruct.to_string buf ~off:0 ~len:n);
          aux (i + avail)
        ) else (
          match t.state with
          | `Open _ ->
            Condition.await cond; 
            aux i
          | _ -> Ok ()
        )
    in
    catch_cancel @@ fun () ->
    let th = aux 0 in
    (* Lwt_eio.Promise.await_lwt @@ *)
    (* Lwt_switch.add_hook_or_exec switch (fun () -> Lwt.cancel th; Lwt.return_unit); *)
    th

let create ~sw ~dir path =
  let fd = Dir.open_out ~sw dir path ~create:(`Or_truncate 0o666) in
  let cond = Condition.create () in
  {
    state = `Open ((fd :> <Flow.source; Flow.sink; Flow.close>), cond);
    dir;
    len = 0;
  }

let finish t =
  match t.state with
  | `Finished -> invalid_arg "Log is already finished!"
  | `Open (fd, cond) ->
    t.state <- `Finished;
    Flow.close fd;
    Condition.broadcast cond;
  | `Readonly _ ->
    t.state <- `Finished
  | `Empty -> () (* Empty can be reused *)

let write t data =
  match t.state with
  | `Finished -> invalid_arg "write: log is finished!"
  | `Readonly _ | `Empty -> invalid_arg "Log is read-only!"
  | `Open (fd, cond) ->
    let len = String.length data in
    Os.write_all fd (Cstruct.of_string data) 0 len;
    t.len <- t.len + len;
    Condition.broadcast cond

let of_saved dir path =
  let stat = Eio_unix.run_in_systhread @@ fun () -> Unix.lstat path in 
  {
    state = `Readonly path;
    dir;
    len = stat.st_size;
  }

let printf t fmt =
  Fmt.kstr (write t) fmt

let empty dir = {
  state = `Empty;
  dir;
  len = 0;
}

let copy ~src ~dst =
  let buf = Cstruct.create 4096 in
  let rec aux () =
    match Eio.Flow.read src buf with
    | 0 -> ()
    | n -> 
      write dst (Cstruct.to_string buf ~off:0 ~len:n);
      aux ()
  in
  aux ()
