include Obuilder.S.STORE

val with_store : dir:Eio.Fs.dir Eio.Path.t -> process:Eio.Process.mgr -> (t -> 'a) -> 'a
(** [with_store t fn] runs [fn] with a fresh store, which is deleted when [fn] returns. *)

val path : t -> Obuilder.S.id -> Eio.Fs.dir Eio.Path.t
(** [path t id] is the path that [id] is or would be stored at. *)

val find : output:string -> t -> Obuilder.S.id option
(** [find ~output t] returns the ID of a build whose "rootfs/output" file contains [output], if any. *)

val delay_store : (unit Eio.Promise.t) ref 
(** Wait for this to resolve after a build function finishes, but before handling the result. *)
