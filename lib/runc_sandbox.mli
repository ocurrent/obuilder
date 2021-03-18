(** Sandbox builds using runc Linux containers. *)

include S.SANDBOX

type config [@@deriving sexp]
(** The type of sandbox configurations *)

val cmdliner : config Cmdliner.Term.t 
(** [cmdliner] is used for command-line interfaces to generate the necessary flags 
    and parameters to setup a specific sandbox's configuration. *)

val create : ?state_dir:string -> config -> t Lwt.t   
(** [create ?state_dir config] generates a new sandbox -- the state directory is used for 
    runc environments where the store's state directory can be passed in, otherwise just leave 
    it out. *)
