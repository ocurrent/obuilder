include Obuilder.S.STORE

val with_store : (t -> 'a) -> 'a
(** [with_store t fn] runs [fn] with a fresh store, which is deleted when [fn] returns. *)

val path : t -> Obuilder.S.id -> string
(** [path t id] is the path that [id] is or would be stored at. *)

val find : output:string -> t -> Obuilder.S.id option
(** [find ~output t] returns the ID of a build whose "rootfs/output" file contains [output], if any. *)

val already_resolved : unit Eio.Promise.t
(** An already-resolved promise for use as a default. *)

val delay_store : (unit Eio.Promise.t) ref
(** Wait for this to resolve after a build function finishes, but before handling the result. *)
