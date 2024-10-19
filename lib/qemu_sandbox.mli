(** Sandbox builds using Docker. *)

include S.SANDBOX

type config [@@deriving sexp]
(** The type of sandbox configurations *)

val cmdliner : config Cmdliner.Term.t
(** [cmdliner] is used for command-line interfaces to generate the
    necessary flags and parameters to setup a specific sandbox's
    configuration. *)

val create : config -> t Lwt.t
(** [create config] is a Docker sandboxing system that is configured
    using [config]. *)
