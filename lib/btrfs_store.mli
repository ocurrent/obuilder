(** Store build results as Btrfs subvolumes. *)

include S.STORE

val create : Eio.Process.mgr -> Eio.Fs.dir Eio.Path.t -> t
(** [create path] is a new store in btrfs directory [path]. *)
