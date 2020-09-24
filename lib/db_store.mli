module Make (Raw : S.STORE) : sig
  type t

  val build :
    t -> ?base:S.id ->
    id:S.id ->
    log:S.logger ->
    (log:Build_log.t -> string -> (unit, [`Msg of string]) Lwt_result.t) ->
    (S.id, [`Msg of string]) Lwt_result.t

  val result : t -> S.id -> string option

  val wrap : Raw.t -> t
end
