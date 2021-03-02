(** Store build results as ZFS snapshots. *)

include S.STORE

val create : pool:string -> t Lwt.t
(** [create ~pool] is a new store in zfs pool [pool]. *)
