(** Store build results using qemu-img. *)

include S.STORE

val create : root:string -> t Lwt.t
(** [create ~root] creates a new QEMU store directory where everything will
    be stored under [root]. *)
