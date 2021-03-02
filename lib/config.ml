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
}

let v ~cwd ~argv ~hostname ~user ~env ~mounts ~network =
  { cwd; argv; hostname; user; env; mounts; network }
