type t = {
  to_file : out_channel;
}

let with_log path fn =
  let to_file = open_out path in
  let t = { to_file } in
  Lwt.finalize
    (fun () -> fn t)
    (fun () -> close_out to_file; Lwt.return_unit)

let write t buf ofs len =
  output t.to_file buf ofs len;
  output stdout buf ofs len;
  flush stdout;
  Lwt.return_unit
