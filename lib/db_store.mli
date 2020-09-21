module Make (Raw : S.STORE) : sig
  type t

  val build :
    t -> ?base:S.id ->
    id:S.id ->
    log:out_channel ->
    (log:Build_log.t -> string -> (unit, 'e) Lwt_result.t) ->
    (unit, 'e) Lwt_result.t

  val wrap : Raw.t -> t
end
