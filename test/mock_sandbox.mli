include Obuilder.S.SANDBOX

val create : unit -> t
val expect :
  t -> (cancelled:unit Eio.Promise.t ->
        ?stdin:Eio_unix.source ->
        log:Obuilder.Build_log.t ->
        Obuilder.Config.t ->
        Eio.Fs.dir Eio.Path.t ->
        (unit, [`Msg of string | `Cancelled]) result Eio.Promise.t) ->
  unit
