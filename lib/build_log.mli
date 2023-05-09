type t
(** The log for a single build step. *)

(** {2 Creating logs} *)

val create : sw:Eio.Switch.t -> Eio.Fs.dir Eio.Path.t -> t
(** [create path] creates a new log file at temporary location [path].
    Call [finish] when done to release the file descriptor. *)

val finish : t -> unit
(** [finish t] marks log [t] as finished.
    If it was open for writing, this closes the file descriptor.
    It cannot be used after this (for reading or writing), although existing
    background operations (e.g. [tail]) can continue successfully. *)

val write : t -> string -> unit
(** [write t data] appends [data] to the log. *)

val printf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [printf t fmt] is a wrapper for [write t] that takes a format string. *)

(** {2 Reading logs} *)

val empty : t
(** [empty] is a read-only log with no content. *)

val of_saved : Eio.Fs.dir Eio.Path.t -> t
(** [of_saved ~fs path] is a read-only log which reads from [path]. *)

val tail : ?switch:Lwt_switch.t -> t -> (string -> unit) -> (unit, [> `Cancelled]) result
(** [tail t dst] streams data from the log to [dst].
    This can be called at any time before [finish] is called.
    @param switch Abort if this is turned off. *)

(* {2 Copying to logs} *)

val copy : src:Eio_unix.source -> dst:t -> unit
(** [copy ~src ~dst] reads bytes from the [src] file descriptor and
    writes them to the build log [dst]. *)
