type id = string

module type STORE = sig
  type t

  val build :
    t -> ?base:id ->
    id:string ->
    log:out_channel ->
    (string -> (unit, 'e) Lwt_result.t) ->
    (unit, 'e) Lwt_result.t
  (** [build t ~id ~log fn] displays the log for build [id] on [log].
      If it doesn't exist yet in the store, it runs [fn tmpdir] to create
      it first. On success, [tmpdir] is saved as [id], which can be used
      as the [base] for further builds, until it is expired from the cache.
      On failure, nothing is recorded and calling [build] again will make
      another attempt at building it.
      @param base Initialise [tmpdir] as a clone of [base] (with any log removed). *)

  (* val path : t -> ID.t -> string *)

  val state_dir : t -> string
  (** [state_dir] is the path of a directory which can be used to store mutable
      state related to this store (e.g. an sqlite3 database). *)
end

module type SANDBOX = sig
  type t

  module Config : sig
    type t

    val v :
      cwd:string ->
      argv:string list ->
      hostname:string ->
      user:Spec.user ->
      env:Os.env ->
      t
  end

  val run : ?stdin:Os.unix_fd -> t -> Config.t -> string -> (unit, 'a) Lwt_result.t
  (** [run t config dir] runs the operation [config] in a sandbox with root
      filesystem [rootfs]. *)
end
