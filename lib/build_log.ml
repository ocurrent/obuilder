let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of Unix.file_descr * Eio.Condition.t * Eio.Mutex.t
    | `Readonly of string
    | `Empty
    | `Finished of string option  (* log file path, if the log was created from a file *)
  ];
  path : string option;   (* the log file path, if known *)
  mutable len : int;
}

let with_dup fd fn =
  let fd2 = Unix.dup ~cloexec:true fd in
  Fun.protect
    (fun () -> fn fd2)
    ~finally:(fun () -> (try Unix.close fd2 with Unix.Unix_error _ -> ()))

let tail ?cancelled t dst =
  let is_cancelled () =
    match cancelled with
    | Some p -> Eio.Promise.is_resolved p
    | None -> false
  in

  let rec readonly_tail fd buf =
    if is_cancelled () then Error `Cancelled
    else
      match Unix.read fd buf 0 max_chunk_size with
      | 0 -> Ok ()
      | n -> dst (Bytes.sub_string buf 0 n); readonly_tail fd buf
  in

  let rec open_tail fd cond mutex buf i =
    if is_cancelled () then Error `Cancelled
    else
      let avail = min (t.len - i) max_chunk_size in
      if avail > 0 then (
        let n = ExtUnix.All.all_pread fd i buf 0 avail in
        dst (Bytes.sub_string buf 0 n);
        open_tail fd cond mutex buf (i + avail)
      ) else (
        match t.state with
        | `Open _ ->
          Eio.Mutex.lock mutex;
          Fun.protect
            (fun () -> Eio.Condition.await cond mutex)
            ~finally:(fun () -> Eio.Mutex.unlock mutex);
          open_tail fd cond mutex buf i
        | `Readonly _ | `Empty | `Finished _ -> Ok ()
      )
  in

  match t.state with
  | `Finished (Some path) ->
    (* Build completed before we started tailing. Read the saved log file. *)
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect
      (fun () ->
         let buf = Bytes.create max_chunk_size in
         readonly_tail fd buf)
      ~finally:(fun () -> (try Unix.close fd with Unix.Unix_error _ -> ()))
  | `Finished None | `Empty -> Ok ()
  | `Readonly path ->
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect
      (fun () ->
         let buf = Bytes.create max_chunk_size in
         readonly_tail fd buf)
      ~finally:(fun () -> (try Unix.close fd with Unix.Unix_error _ -> ()))
  | `Open (fd, cond, mutex) ->
    with_dup fd @@ fun fd ->
    let buf = Bytes.create max_chunk_size in
    match cancelled with
    | None -> open_tail fd cond mutex buf 0
    | Some cancelled_p ->
      (* Race between tailing the log and being cancelled.
         We need Fiber.first because open_tail blocks on the condition,
         and won't check is_cancelled until woken up. *)
      Eio.Fiber.first
        (fun () -> open_tail fd cond mutex buf 0)
        (fun () ->
          Eio.Promise.await cancelled_p;
          (* Wake up open_tail so it can be cancelled cleanly *)
          Eio.Condition.broadcast cond;
          Error `Cancelled)

let create path =
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR; Unix.O_CLOEXEC] 0o666 in
  let cond = Eio.Condition.create () in
  let mutex = Eio.Mutex.create () in
  {
    state = `Open (fd, cond, mutex);
    path = Some path;
    len = 0;
  }

let finish t =
  match t.state with
  | `Finished _ -> invalid_arg "Log is already finished!"
  | `Open (fd, cond, _mutex) ->
    t.state <- `Finished t.path;
    Unix.close fd;
    Eio.Condition.broadcast cond
  | `Readonly _ ->
    t.state <- `Finished t.path
  | `Empty ->
    () (* Empty can be reused *)

let write t data =
  match t.state with
  | `Finished _ -> invalid_arg "write: log is finished!"
  | `Readonly _ | `Empty -> invalid_arg "Log is read-only!"
  | `Open (fd, cond, _mutex) ->
    let len = String.length data in
    Os.write_all_string fd data 0 len;
    t.len <- t.len + len;
    Eio.Condition.broadcast cond

let of_saved path =
  let stat = Unix.lstat path in
  {
    state = `Readonly path;
    path = Some path;
    len = stat.st_size;
  }

let printf t fmt =
  Fmt.kstr (write t) fmt

let empty = {
  state = `Empty;
  path = None;
  len = 0;
}

let copy ~src ~dst =
  let buf = Bytes.create max_chunk_size in
  let rec aux () =
    match Unix.read src buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n -> write dst (Bytes.sub_string buf 0 n); aux ()
  in
  aux ()
