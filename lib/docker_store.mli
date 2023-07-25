(** Store build results as Docker images. *)

include S.STORE

val create : string -> t Lwt.t
(** [create root] is a new store using Docker images and [root] to store
    ancillary state. *)

val cache_stats : t -> int * int
