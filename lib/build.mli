module Context : sig
  type t

  val default_env : Os.env

  val v :
    ?env:Os.env ->
    ?user:Spec.user ->
    ?workdir:string ->
    ?shell:string list ->
    src_dir:string ->
    unit -> t
    (** [context ~src_dir] is a build context where copy operations read from the (host) directory [src_dir].
        @param env Environment in which to run commands.
        @param user Container user to run as.
        @param workdir Directory in the container namespace for cwd.
        @param shell The command used to run shell commands (default [["/bin/bash"; "-c"]]).
    *)
end

module Make (Store : S.STORE) (Sandbox : S.SANDBOX) : sig
  type t

  val v : store:Store.t -> sandbox:Sandbox.t -> t

  val build :
    t ->
    Context.t ->
    Spec.stage ->
    (Store.ID.t, [> ]) Lwt_result.t
end
