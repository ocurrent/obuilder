open Eio

let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of <Eio.Flow.sink; Eio.Flow.source; Eio.Flow.close> * Condition.t  (* Fires after writing more data. *)
    | `Readonly of Eio.Fs.dir Eio.Path.t
    | `Empty
    | `Finished
  ];
  mutable len : int;
}

let with_dup ~sw fd fn =
  match Eio_unix.Resource.fd_opt fd with
  | None -> failwith "Expected backing file descriptor"
  | Some fd ->
    Eio_unix.Fd.use_exn "with-dup" fd @@ fun fd ->
    let copy = Unix.dup fd in 
    let sock = Eio_unix.import_socket_stream ~sw ~close_unix:true copy in
    fn sock

let catch_cancel fn =
  try fn () with
    | Cancel.Cancelled _ -> 
      Logs.info (fun f -> f "Catch cancel");
      Error `Cancelled
    | ex -> raise ex

let tail ?switch t dst =
  match t.state with
  | `Finished -> invalid_arg "tail: log is finished!"
  | `Readonly path ->
    (* let flags = [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in *)
    Path.(with_open_in path) @@ fun ch ->
    (* Lwt_io.(with_file ~mode:input ~flags) path @@ fun ch -> *)
    let buf = Cstruct.create max_chunk_size in
    catch_cancel @@ fun () ->
    let th, cancel =
      let th, set_th = Promise.create () in
      Switch.run @@ fun sw ->
      let rec aux () =
        try 
          match Flow.single_read ch (Cstruct.sub buf 0 max_chunk_size) with
          | 0 -> Promise.resolve set_th (Ok ())
          | n -> dst (Cstruct.to_string buf ~off:0 ~len:n); aux ()
        with End_of_file -> Promise.resolve set_th (Ok ())
      in
      let cancel () =
        match Promise.peek th with
        | Some _ -> ()
        | None -> 
          Switch.fail sw (Failure "cancelled");
          Promise.resolve_error set_th `Cancelled
      in
      Fiber.fork ~sw aux;
      th, cancel
    in
    Lwt_eio.Promise.await_lwt @@
    Lwt_switch.add_hook_or_exec switch (fun () -> cancel (); Lwt.return_unit);
    Promise.await th
  | `Empty -> Ok ()
  | `Open (fd, cond) ->
    (* Dup [fd], which can still work after [fd] is closed. *)
    catch_cancel @@ fun () ->
    Switch.run @@ fun sw ->
    with_dup ~sw fd @@ fun fd ->
    let buf = Cstruct.create max_chunk_size in
    let th, aux, cancel =
      let th, set_th = Promise.create () in
      let rec aux i =
        Switch.check sw;
        Logs.info (fun f -> f "AUX %i" i);
        match switch with 
        | Some sw when not (Lwt_switch.is_on sw) -> Error `Cancelled
        | _ ->
        let avail = min (t.len - i) max_chunk_size in
        if avail > 0 then (
          let n = Flow.single_read fd buf in
          dst (Cstruct.to_string buf ~off:0 ~len:n);
          aux (i + avail)
        ) else (
          match t.state with
          | `Open _ ->
            Condition.await_no_mutex cond; 
            aux i
          | _ -> Ok ()
        )
      in
      let cancel () =
        match Promise.peek th with
        | Some _ -> ()
        | None -> 
          Promise.resolve_error set_th `Cancelled;
          Switch.fail sw (Cancel.Cancelled (Failure "cancelled"))
      in
      th, (fun () -> Fiber.fork ~sw (fun () -> Promise.resolve set_th @@ aux 0)), cancel
    in
    let () = aux () in
    Lwt_eio.Promise.await_lwt @@
    Lwt_switch.add_hook_or_exec switch (fun () -> cancel (); Lwt.return_unit);
    let r = Promise.await th in
    Logs.info (fun f -> f "Exiting %s" (match r with Ok () -> "OK" | _ -> "CAncelled"));
    r

let create ~sw path =
  let fd = Path.(open_out ~sw path ~create:(`Or_truncate 0o666)) in
  let cond = Condition.create () in
  {
    state = `Open ((fd :> <Eio.Flow.sink; Eio.Flow.source; Eio.Flow.close>), cond);
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

let of_saved path =
  Path.(with_open_in path) @@ fun f ->
  let stat = File.stat f in
  {
    state = `Readonly path;
    len = Optint.Int63.to_int stat.size;
  }

let printf t fmt =
  Fmt.kstr (write t) fmt

let empty = {
  state = `Empty;
  len = 0;
}

let copy ~src ~dst =
  let buf = Cstruct.create 4096 in
  let rec aux () =
    match Eio.Flow.single_read src buf with
    | 0 -> ()
    | n -> 
      write dst (Cstruct.to_string buf ~off:0 ~len:n);
      aux ()
  in
  aux ()
