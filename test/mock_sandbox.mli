include Obuilder.S.SANDBOX

val create : unit -> t
val expect :
  t -> (cancelled:unit Eio.Promise.t ->
        ?stdin:Obuilder.Os.unix_fd ->
        log:Obuilder.Build_log.t ->
        Obuilder.Config.t ->
        string ->
        (unit, [`Msg of string | `Cancelled]) result) ->
  unit
val finished : unit -> unit
