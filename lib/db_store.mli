open Eio

module Make (Raw : S.STORE) : sig
  type t

  val build :
    ?switch:Lwt_switch.t ->
    t -> ?base:S.id ->
    id:S.id ->
    log:S.logger ->
    (cancelled:unit Promise.t -> log:Build_log.t -> Eio.Fs.dir Eio.Path.t -> (unit, [`Cancelled | `Msg of string]) result) ->
    (S.id, [`Cancelled | `Msg of string]) result
  (** [build t ~id ~log fn] ensures that [id] is cached, using [fn ~cancelled ~log dir] to build it if not.
      If [cancelled] resolves, the build should be cancelled.
      If [id] is already in the process of being built, this just attaches to the existing build.
      @param switch Turn this off if you no longer need the result. The build
                    will be cancelled if no-one else is waiting for it. *)

  val delete : ?log:(S.id -> unit) -> t -> S.id -> unit

  val prune : ?log:(S.id -> unit) -> t -> before:Unix.tm -> int -> int

  val result : t -> S.id -> Eio.Fs.dir Eio.Path.t option

  val cache :
    user : Obuilder_spec.user ->
    t ->
    string ->
    (Eio.Fs.dir Eio.Path.t * (unit -> unit))

  val wrap : Raw.t -> t
end
