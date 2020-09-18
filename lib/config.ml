type t = {
  cwd : string;
  argv : string list;
  hostname : string;
  user : Spec.user;
  env : Os.env;
}

let v ~cwd ~argv ~hostname ~user ~env =
  { cwd; argv; hostname; user; env }
