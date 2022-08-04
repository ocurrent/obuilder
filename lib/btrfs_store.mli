(** Store build results as Btrfs subvolumes. *)

include S.STORE

val create : Eio.Process.t -> string -> t
(** [create path] is a new store in btrfs directory [path]. *)
