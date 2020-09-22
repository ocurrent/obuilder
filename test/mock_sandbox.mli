include Obuilder.S.SANDBOX

val create : string -> t
val expect : t -> (?stdin:Obuilder.Os.unix_fd -> log:Obuilder.Build_log.t -> Obuilder.Config.t -> string -> unit Lwt.t) -> unit
