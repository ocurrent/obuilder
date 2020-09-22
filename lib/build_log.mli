type t
(** The log for a single build step. *)

(** {2 Creating logs} *)

val create : string -> t Lwt.t
(** [create path] creates a new log file at temporary location [path].
    Call [finish] when done to release the file descriptor. *)

val finish : t -> unit Lwt.t
(** [finish t] marks log [t] as finished.
    If it was open for writing, this closes the file descriptor.
    It cannot be used after this (for reading or writing), although existing
    background operations (e.g. [tail]) can continue successfully. *)

val write : t -> string -> unit Lwt.t
(** [write t data] appends [data] to the log. *)

val printf : t -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
(** [printf t fmt] is a wrapper for [write t] that takes a format string. *)

(** {2 Reading logs} *)

val empty : t
(** [empty] is a read-only log with no content. *)

val of_saved : string -> t Lwt.t
(** [of_saved path] is a read-only log which reads from [path]. *)

val tail : t -> (string -> unit) -> unit Lwt.t
(** [tail t dst] streams data from the log to [dst].
    This can be called at any time in the lifecycle of the log. *)
