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
      @param base Initialise [tmpdir] as a clone of [base]. *)

  val delete : t -> id -> unit Lwt.t
  (** [delete t id] removes [id] from the store, if present. *)

  val result : t -> id -> string option
  (** [result t id] is the path of the build result for [id], if present. *)

  val state_dir : t -> string
  (** [state_dir] is the path of a directory which can be used to store mutable
      state related to this store (e.g. an sqlite3 database). *)
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
      filesystem [rootfs].
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
    Spec.stage ->
    (id, [`Cancelled | `Msg of string]) Lwt_result.t

  val delete : ?log:(id -> unit) -> t -> id -> unit Lwt.t
  (** [delete ?log t id] removes [id] from the store, along with all of its dependencies.
      This is for testing. Note that is not safe to perform builds while deleting:
      the delete might fail because an item got a new child during the delete, or
      we might delete something that the build is using.
      @param log Called just before deleting each item, so it can be displayed. *)
end
