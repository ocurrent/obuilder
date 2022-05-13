(** Store build results using rsync. *)

include S.STORE

type mode =
  | Copy (** Fast but uses more disk space. *)
  | Hardlink (** Slow but consumes less disk space. *)
  | Hardlink_unsafe (** Reasonnably fast and uses less disk space, but no
                        checksum verification. Only for testing during
                        development, do not use in production. *)

val create : path:string -> ?mode:mode -> unit -> t Lwt.t
(** [create ~path ?mode ()] creates a new rsync store where everything will
    be stored under [path]. The [mode] defaults to [Copy] and defines how
    the caches are reused: [Copy] copies all the files, while [Hardlink] tries
    to save disk space by sharing identical files. *)
