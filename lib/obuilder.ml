let log_src = Log.src

(** {2 Types} *)

module S = S
module Spec = Obuilder_spec
module Context = Build.Context
module Docker = Docker

(** {2 Stores} *)

module Btrfs_store = Btrfs_store
module Zfs_store = Zfs_store
module Rsync_store = Rsync_store
module Store_spec = Store_spec
module Docker_store = Docker_store

(** {2 Fetchers} *)
module User_temp = User_temp
module Docker_extract = Docker.Extract
module Archive_extract = Archive_extract

(** {2 Sandboxes} *)

module Config = Config
module Native_sandbox = Sandbox
module Docker_sandbox = Docker_sandbox

(** {2 Builders} *)

module type BUILDER = S.BUILDER with type context := Build.Context.t
module Builder = Build.Make
module Docker_builder = Build.Make_Docker
module Build_log = Build_log

(**/**)

(* For unit-tests *)
module Manifest = Manifest
module Escape = Escape
module Os = Os
module Db = Db
module Tar_transfer = Tar_transfer
