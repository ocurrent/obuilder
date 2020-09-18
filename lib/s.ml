type id = string

module type STORE = sig
  type t

  val build :
    t -> ?base:id ->
    id:string ->
    log:out_channel ->
    (log:Build_log.t -> string -> (unit, 'e) Lwt_result.t) ->
    (unit, 'e) Lwt_result.t
  (** [build t ~id ~log fn] displays the log for build [id] on [log].
      If it doesn't exist yet in the store, it runs [fn ~log tmpdir] to create
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

  type error = private [> ]

  val pp_error : error Fmt.t

  val run : ?stdin:Os.unix_fd -> log:Build_log.t -> t -> Config.t -> string -> (unit, error) Lwt_result.t
  (** [run t config dir] runs the operation [config] in a sandbox with root
      filesystem [rootfs].
      @param stdin Passed to child as its standard input.
      @param log Used for child's stdout and stderr.
  *)
end
