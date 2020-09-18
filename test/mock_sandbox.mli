include Obuilder.S.SANDBOX with
  type error = [`Exn of exn]

val create : string -> t
val expect : t -> (?stdin:Obuilder.Os.unix_fd -> Obuilder.Config.t -> string -> unit Lwt.t) -> unit
