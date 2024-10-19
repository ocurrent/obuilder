(** Store build results using qemu-img. *)

include S.STORE

val create : root:string -> t Lwt.t
(** [create ~path] creates a new overlayfs store where everything will
    be stored under [path]. *)
