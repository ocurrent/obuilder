include S.SANDBOX

val create : ?fast_sync:bool -> runc_state_dir:string -> unit -> t
(** [create dir] is a runc sandboxing system that keeps state in [dir].
    @param fast_sync Use seccomp to skip all sync syscalls. This is fast (and
                     safe, since we discard builds after a crash), but requires
                     runc version 1.0.0-rc92 or later. Note that the runc version
                     is not the same as the spec version. If "runc --version"
                     only prints the spec version, then it's too old. *)
