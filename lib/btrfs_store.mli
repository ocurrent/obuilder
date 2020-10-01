include S.STORE

val create : string -> t Lwt.t
(** [create path] is a new store in btrfs directory [path]. *)
