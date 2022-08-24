include Obuilder.S.SANDBOX

val create : unit -> t
val expect :
  t -> (cancelled:unit Eio.Promise.t ->
        ?stdin:<Eio.Flow.source; Eio_unix.unix_fd> ->
        log:Obuilder.Build_log.t ->
        Obuilder.Config.t ->
        string ->
        (unit, [`Msg of string | `Cancelled]) result Eio.Promise.t) ->
  unit
