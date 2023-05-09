open Eio

type t = {
  expect :
    (cancelled:unit Promise.t ->
     ?stdin:Eio_unix.source ->
     log:Obuilder.Build_log.t ->
     Obuilder.Config.t ->
     Eio.Fs.dir Eio.Path.t ->
     (unit, [`Msg of string | `Cancelled]) result Promise.t) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ~dir:_ ~process:_ ~cancelled ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    try
      Promise.await @@ fn ~cancelled ?stdin ~log config dir
    with
      | Failure ex -> Error (`Msg ex)
      | ex -> Error (`Msg (Printexc.to_string ex))

let create () = { expect = Queue.create () }
