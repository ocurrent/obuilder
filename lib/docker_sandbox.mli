(** Sandbox builds using Docker. *)

include S.SANDBOX

val teardown : log:Build_log.t -> commit:bool -> S.id -> unit Lwt.t

val manifest_from_build :
  t ->
  base:S.id ->
  exclude:string list -> string list -> string -> Obuilder_spec.user ->
  (Manifest.t list, [> `Msg of string ]) Lwt_result.t

val copy_from_context :
  t ->
  cancelled:unit Lwt.t ->
  log:Build_log.t ->
  [< `Copy_item of Manifest.t * string
  | `Copy_items of Manifest.t list * string ] ->
  user:Obuilder_spec.user ->
  src_dir:string ->
  ?dst_dir:string ->
  string -> (unit, [ `Cancelled | `Msg of string ]) result Lwt.t

val copy_from_build :
  t ->
  cancelled:'a Lwt.t ->
  log:Build_log.t ->
  [< `Copy_item of Manifest.t * string
  | `Copy_items of Manifest.t list * string ] ->
  user:Obuilder_spec.user ->
  workdir:string ->
  ?dst_dir:string ->
  from_id:S.id ->
  S.id ->
  (unit, [ `Cancelled | `Msg of string ]) result Lwt.t

val servercore : unit -> string Lwt.t
(** Get the Windows ServerCore image based on the same version as the
    host. *)

module Docker_config : sig
  val make : Config.t -> ?config_dir:string -> t -> string list * string list
  (** [make obuilder_config ~config_dir sandbox_config] returns
      [docker_argv, argv] where [docker_argv] is the list of arguments
      to give to the Docker command-line client, and [argv] the command
      to execute in the container. *)
end
(** Derive Docker command-line client parameters from an OBuilder
    configuration. *)

type config [@@deriving sexp]
(** The type of sandbox configurations *)

val cmdliner : config Cmdliner.Term.t
(** [cmdliner] is used for command-line interfaces to generate the
    necessary flags and parameters to setup a specific sandbox's
    configuration. *)

val create : config -> t Lwt.t
(** [create config] is a Docker sandboxing system that is configured
    using [config]. *)
