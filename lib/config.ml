(** Configuration for a single sandboxed build step.
    This is passed by the builder to the sandbox. *)

open Sexplib.Std

type env = (string * string) list [@@deriving sexp]

module Mount = struct
  type t = { (* TODO: options *)
    src : string;              (* In host namespace *)
    dst : string;              (* In container namespace *)
  }
end

type t = {
  cwd : string;
  argv : string list;
  hostname : string;
  user : Obuilder_spec.user;
  env : env;
  mounts : Mount.t list;
  network : string list;
  mount_secrets : (string * string * string) list; (* key, value, target *)
}

let v ~cwd ~argv ~hostname ~user ~env ~mounts ~network ~mount_secrets =
  { cwd; argv; hostname; user; env; mounts; network; mount_secrets }
