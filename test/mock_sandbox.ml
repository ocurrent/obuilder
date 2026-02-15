
include Obuilder.S.Sandbox_default

type t = {
  expect :
    (cancelled:unit Eio.Promise.t ->
     ?stdin:Obuilder.Os.unix_fd ->
     log:Obuilder.Build_log.t ->
     Obuilder.Config.t ->
     string ->
     (unit, [`Msg of string | `Cancelled]) result) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ~cancelled ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    try fn ~cancelled ?stdin ~log config dir
    with
    | Failure ex -> Error (`Msg ex)
    | ex -> Error (`Msg (Printexc.to_string ex))

let create () = { expect = Queue.create () }

let finished () = ()
