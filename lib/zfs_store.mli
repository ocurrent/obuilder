(** Store build results as ZFS snapshots. *)

include S.STORE

val create : fs:Eio.Fs.dir Eio.Path.t -> process:Eio.Process.mgr -> pool:string -> t
(** [create ~pool] is a new store in zfs pool [pool]. *)
