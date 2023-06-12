(** Sandbox builds. *)

include S.SANDBOX

type config [@@deriving sexp]
(** The type of sandbox configurations *)

val cmdliner : config Cmdliner.Term.t
(** [cmdliner] is used for command-line interfaces to generate the necessary flags
    and parameters to setup a specific sandbox's configuration. *)

val create : state_dir:string -> config -> t Lwt.t
(** [create ~state_dir config] is a sandboxing system that keeps state in [state_dir]
    and is configured using [config]. *)

val finished : unit -> unit Lwt.t
(** [finished] is a call back to the sandbox which is triggered when the current job
    is finished. The sandbox may choose do nothing. *)
