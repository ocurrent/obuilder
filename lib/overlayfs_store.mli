(** Store build results using rsync. *)

include S.STORE

val create : path:string -> t Lwt.t
(** [create ~path] creates a new overlayfs store where everything will
    be stored under [path]. *)
