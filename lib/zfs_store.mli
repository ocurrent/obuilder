(** Store build results as ZFS snapshots. *)

include S.STORE

val create : process:Eio.Process.t -> pool:string -> t
(** [create ~pool] is a new store in zfs pool [pool]. *)
