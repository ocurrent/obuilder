type config = { dir : string } [@@derivign sexp]
(** Exposing the configuration so testing can generate them rather than 
    relying on cmdliner *)

include Obuilder.S.SANDBOX with type config := config

val mock_create : config -> t
(** To simplify test sandbox creation, this is an Lwt free [create] function *)

val expect :
  t -> (cancelled:unit Lwt.t ->
        ?stdin:Obuilder.Os.unix_fd ->
        log:Obuilder.Build_log.t ->
        Obuilder.Config.t ->
        string ->
        (unit, [`Msg of string | `Cancelled]) Lwt_result.t) ->
  unit
