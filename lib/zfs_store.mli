(** Store build results as ZFS snapshots. *)

include S.STORE

val create : path:string -> t Lwt.t
(** [create ~path] creates a new zfs store in a pool mounted at [path].
    The pool name is [Filename.basename path]. If only a poolname is passed
    such as [tank] the path is inferred as [/tank].*)
