let log_src = Log.src

(** {2 Types} *)

module S = S
module Spec = Obuilder_spec
module Context = Build.Context

(** {2 Stores} *)

module Btrfs_store = Btrfs_store
module Zfs_store = Zfs_store
module Docker_store = Docker_store
module Store_spec = Store_spec

(** {2 Fetchers} *)
module Docker_extract = Docker.Extract
module Docker_pull = Docker.Pull

(** {2 Sandboxes} *)

module Config = Config
module Docker_sandbox = Docker_sandbox
module Runc_sandbox = Runc_sandbox

(** {2 Builders} *)

module type BUILDER = S.BUILDER with type context := Build.Context.t
module Builder = Build.Make
module Docker_builder = Build.MakeDocker
module Build_log = Build_log

(**/**)

(* For unit-tests *)
module Manifest = Manifest
module Escape = Escape
module Os = Os
module Db = Db
