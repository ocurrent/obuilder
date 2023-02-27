(** Docker interface over the CLI tool  *)

type ids = [
  | `Docker_container of string | `Docker_image of string
  | `Docker_volume of string
  | `Obuilder_id of string
]

val set_prefix : string -> unit
(** Set the prefix for Docker images, containers, and volumes managed
    by the current OBuilder instance. *)

val obuilder_libexec : unit -> string
val obuilder_libexec_volume : ?readonly:bool -> unit -> Config.Mount.t

val image_name : ?tmp:bool -> S.id -> string
val container_name : S.id -> string
val volume_copy_name : ?tmp:bool -> S.id -> string

val docker_image : ?tmp:bool -> S.id -> [> `Docker_image of string ]
val docker_container : S.id -> [> `Docker_container of string ]
val docker_volume_cache : ?tmp:bool -> S.id -> [> `Docker_volume of string ]
val docker_volume_copy : ?tmp:bool -> S.id -> [> `Docker_volume of string ]

val mount_point_inside_unix : string
(** Mount point of Docker volumes inside Docker containers, Unix path
    style. Use with Cygwin tools. *)

val mount_point_inside_native : string
(** Mount point of Docker volumes inside Docker containers, native
    path style. *)

(** Get the CLI arguments to the Docker client to mount a volume. *)
val mount_args : Config.Mount.t -> string list

val bash_entrypoint : string -> string list
(** Get a Bash entrypoint in a Docker container to execute Bash
    scripts. *)

val default_entrypoint : string list
(** Get the default entrypoint of Docker container according to the
    host (Windows is cmd, everywhere else is sh). *)

val setup_command : entp:string list -> cmd:string list -> string * string list
(** [setup_command ~entp ~cmd] returns the head of [entp], to be
    give to Docker's [--entrypoint], and the concatenation of the tail
    of [head] and [cmd] to be given to Docker command, as Docker
    [--entrypoint] takes only one argument. *)

(** Copy the file [src] to [dst] inside the Docker volume [volume],
    using the [base] Docker image for a temporary container. *)
val cp_to_volume :
  base:[< `Docker_image of string ] ->
  volume:Config.Mount.t ->
  src:string -> dst:string ->
  (unit, [> `Msg of string]) Lwt_result.t

val cp_between_volumes :
  base:[< `Docker_image of string ] ->
  src:[< `Docker_volume of string] -> dst:[`Docker_volume of string] ->
  (unit, [> `Msg of string]) Lwt_result.t

(** Wrappers for various Docker client commands, exposing file descriptors. *)
module Cmd : S.DOCKER_CMD
       with
         type 'a log = ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                       ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                       'a
       and
         type 'a logerr = ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
                          'a

(** Wrappers for various Docker client commands, logging directly to the
    {!Build_log}. *)
module Cmd_log : S.DOCKER_CMD
       with
         type 'a log = log:Build_log.t -> 'a
       and
         type 'a logerr = log:Build_log.t -> 'a

(** Fetch (pull and extract) base images using Docker *)
module Extract : S.FETCHER
