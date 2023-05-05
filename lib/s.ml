open Sexplib.Std

type id = string [@@deriving sexp_of]

type tag = [
  | `Heading    (** Introduces a new build step *)
  | `Note       (** Informational output from OBuilder *)
  | `Output     (** Raw output from the build command *)
]

type logger = tag -> string -> unit

module type STORE = sig
  type t

  val root : t -> string
  (** [root t] returns the root of the store. *)

  val build :
    t -> ?base:id ->
    id:id ->
    (string -> (unit, 'e) Lwt_result.t) ->
    (unit, 'e) Lwt_result.t
  (** [build t ~id fn] runs [fn tmpdir] to add a new item to the store under
      key [id]. On success, [tmpdir] is saved as [id], which can be used
      as the [base] for further builds, until it is expired from the cache.
      On failure, nothing is recorded and calling [build] again will make
      another attempt at building it.
      The builder will not request concurrent builds for the same [id] (it
      will handle that itself). It will also not ask for a build that already
      exists (i.e. for which [result] returns a path).
      @param base Initialise [tmpdir] as a clone of [base]. *)

  val delete : t -> id -> unit Lwt.t
  (** [delete t id] removes [id] from the store, if present. *)

  val result : t -> id -> string option Lwt.t
  (** [result t id] is the path of the build result for [id], if present. *)

  val log_file : t -> id -> string Lwt.t
  (** [log_file t id] is the path of the build logs for [id]. The file may
      not exist if the build has never been run, or failed. *)

  val state_dir : t -> string
  (** [state_dir] is the path of a directory which can be used to store mutable
      state related to this store (e.g. an sqlite3 database). *)

  val cache :
    user:Obuilder_spec.user ->
    t ->
    string ->
    (string * (unit -> unit Lwt.t)) Lwt.t
  (** [cache ~user t name] creates a writeable copy of the latest snapshot of the
      cache [name]. It returns the path of this fresh copy and a function which
      must be called to free it when done.
      If the cache [name] does not exist, it is first created (as an empty directory,
      and owned by [user]).
      When the copy is released, it is snapshotted to become the new latest
      version of the cache, unless the cache has already been updated since
      it was snapshotted, in which case this writeable copy is simply discarded. *)

  val delete_cache : t -> string -> (unit, [> `Busy]) Lwt_result.t
  (** [delete_cache t name] removes the cache [name], if present.
      If the cache is currently in use, the store may instead return [Error `Busy]. *)

  val complete_deletes : t -> unit Lwt.t
  (** [complete_deletes t] attempts to wait for previously executed deletes to finish,
      so that the free space is accurate. *)
end

module type SANDBOX = sig
  type t

  val run :
    cancelled:unit Lwt.t ->
    ?stdin:Os.unix_fd ->
    log:Build_log.t ->
    t ->
    Config.t ->
    string ->
    (unit, [`Cancelled | `Msg of string]) Lwt_result.t
  (** [run ~cancelled t config dir] runs the operation [config] in a sandbox with root
      filesystem [dir].
      @param cancelled Resolving this kills the process (and returns [`Cancelled]).
      @param stdin Passed to child as its standard input.
      @param log Used for child's stdout and stderr.
  *)
end

module type BUILDER = sig
  type t
  type context

  val build :
    t ->
    context ->
    Obuilder_spec.t ->
    (id, [> `Cancelled | `Msg of string]) Lwt_result.t

  val finish : t -> unit Lwt.t
  (** [finish builder] close allocated resources and store state (e.g., sqlite3
      databases). *)

  val delete : ?log:(id -> unit) -> t -> id -> unit Lwt.t
  (** [delete ?log t id] removes [id] from the store, along with all of its dependencies.
      This is for testing. Note that is not safe to perform builds while deleting:
      the delete might fail because an item got a new child during the delete, or
      we might delete something that the build is using.
      @param log Called just before deleting each item, so it can be displayed. *)

  val prune : ?log:(id -> unit) -> t -> before:Unix.tm -> int -> int Lwt.t
  (** [prune t ~before n] attempts to remove up to [n] items from the store,
      all of which were last used before [before].
      Returns the number of items removed.
      @param log Called just before deleting each item, so it can be displayed. *)

  val count : t -> int64
  (** [count t] return number of items in the store. *)

  val healthcheck : ?timeout:float -> t -> (unit, [> `Msg of string]) Lwt_result.t
  (** [healthcheck t] performs a check that [t] is working correctly.
      @param timeout Cancel and report failure after this many seconds.
                     This excludes the time to fetch the base image. *)
end

module type FETCHER = sig
  val fetch : log:Build_log.t -> rootfs:string -> string -> Config.env Lwt.t
  (** [fetch ~log ~rootfs base] initialises the [rootfs] directory by
      fetching and extracting the [base] image.
      Returns the image's environment.
      @param log Used for outputting the progress of the fetch
      @param rootfs The directory in which to extract the base image *)
end

(** Wrappers for various Docker client commands.  *)
module type DOCKER_CMD = sig
  type 'a log
  (** Log standard output and standard error of the sub-process. *)

  type 'a logerr
  (** Log only standard error of the sub-process. *)

  val version : (unit -> (string, [> `Msg of string ]) result Lwt.t) logerr

  val pull :
    ([< `Docker_image of string ] -> unit Lwt.t) log
  (** Pulls a Docker image. *)
  val export :
    ([< `Docker_container of string ] -> unit Lwt.t) log
  (** Exports a Docker container. *)
  val image :
    ([< `Remove of [< `Docker_image of string ] ] -> unit Lwt.t) log
  (** Operates on a Docker image. *)
  val rm :
    ([ `Docker_container of string ] list -> unit Lwt.t) log
  (** Removes a Docker container.  *)
  val rmi :
    ([ `Docker_image of string ] list -> unit Lwt.t) log
  (** Removes a list of Docker images. *)
  val tag :
    ([< `Docker_image of string ] ->
     [< `Docker_image of string ] -> unit Lwt.t) log
  (** [tag source_image target_image] creates a new tag for a Docker iamge. *)
  val commit :
    ([< `Docker_image of string ] ->
     [< `Docker_container of string ] ->
     [< `Docker_image of string ] -> unit Lwt.t) log
  (** [commit base_image container target_image] commits the
      [container] to the [target_image] using [base_image] (typically
      the container's base image) entrypoint and cmd. *)
  val volume :
    (?timeout:float ->
    [< `Create of [< `Docker_volume of string ]
     | `Inspect of [< `Docker_volume of string ] list * [< `Mountpoint ]
     | `List of string option
     | `Remove of [< `Docker_volume of string ] list ] ->
     string Lwt.t) logerr
  (** Operates on Docker volumes. *)
  val volume_containers :
    ([< `Docker_volume of string ] -> [> `Docker_container of string ] list Lwt.t) logerr
  (** [volume_containers vol] returns the list of containers using [vol].  *)
  val mount_point :
    ([< `Docker_volume of string ] -> string Lwt.t) logerr
  (** [mount_point vol] returns the mount point in the host filesystem of [vol]. *)
  val build :
    (string list -> [< `Docker_image of string ] -> string -> unit Lwt.t) log
  (** [build docker_args image context_path] builds the Docker [image]
      using the context located in [context_path]. *)

  val run :
    ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    (?is_success:(int -> bool) ->
     ?name:[< `Docker_container of string ] ->
     ?rm:bool ->
     string list -> [< `Docker_image of string ] -> string list -> unit Lwt.t) log
  (** [run ?stdin ?stdout ?stderr ?is_success ?name ?rm docker_argv image argv] *)
  val run' :
    ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    (?is_success:(int -> bool) ->
     ?name:[< `Docker_container of string ] ->
     ?rm:bool ->
     string list -> [< `Docker_image of string ] -> string list -> unit Lwt.t) logerr
  (** [run' ?stdin ?stdout ?stderr ?is_success ?name ?rm docker_argv image argv] *)
  val run_result :
    ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    (?name:[< `Docker_container of string ] ->
     ?rm:bool ->
     string list ->
     [< `Docker_image of string ] ->
     string list -> (unit, [> `Msg of string ]) result Lwt.t) log
  (** [run_result ?stdin ?stdout ?stderr ?is_success ?name ?rm docker_argv image argv] *)
  val run_result' :
    ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    (?name:[< `Docker_container of string ] ->
     ?rm:bool ->
     string list ->
     [< `Docker_image of string ] ->
     string list -> (unit, [> `Msg of string ]) result Lwt.t) logerr
  (** [run_result ?stdin ?stdout ?stderr ?is_success ?name ?rm docker_argv image argv] *)
  val run_pread_result :
    ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
    (?name:[< `Docker_container of string ] ->
     ?rm:bool ->
     string list ->
     [< `Docker_image of string ] ->
     string list -> (string, [> `Msg of string ]) result Lwt.t) logerr
  (** [run_pread_result ?stdin ?stdout ?stderr ?is_success ?name ?rm docker_argv image argv] *)

  val stop :
    ([< `Docker_container of string ] ->
     (unit, [> `Msg of string ]) result Lwt.t) log
  (** Stop a Docker container. *)

  val manifest :
    ([< `Create of
          [< `Docker_image of string ] * [< `Docker_image of string ] list
     | `Inspect of [< `Docker_image of string ]
     | `Remove of [< `Docker_image of string ] list ] ->
     (unit, [> `Msg of string ]) result Lwt.t) log
  (** Operates on a Docker manifest. *)

  val exists :
    ([< `Docker_container of string
     | `Docker_image of string
     | `Docker_volume of string ] ->
     (unit, [> `Msg of string ]) result Lwt.t) log
  (** Tests if an object exists. *)

  val obuilder_images :
    (?tmp:bool -> unit -> [ `Docker_image of string ] list Lwt.t) logerr
  (** Returns the list of this OBuilder instance images. *)
  val obuilder_containers :
    (unit -> [ `Docker_container of string ] list Lwt.t) logerr
  (** Returns the list of this OBuilder instance containers. *)
  val obuilder_volumes :
    (?prefix:string -> unit -> [ `Docker_volume of string ] list Lwt.t) logerr
  (** Returns the list of this OBuilder instance volumes. *)
  val obuilder_caches_tmp :
    (unit -> [ `Docker_volume of string ] list Lwt.t) logerr
  (** Returns the list of this OBuilder instance temporary caches. *)
end
