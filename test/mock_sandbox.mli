include Obuilder.S.SANDBOX

val create : unit -> t
val expect :
  t -> (cancelled:unit Lwt.t ->
        ?stdin:Obuilder.Os.unix_fd ->
        log:Obuilder.Build_log.t ->
        Obuilder.Config.t ->
        string ->
        (unit, [`Msg of string | `Cancelled]) Lwt_result.t) ->
  unit
