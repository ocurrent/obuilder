(** Docker interface over the CLI tool  *)

type ids = [
  | `Docker_container of string | `Docker_image of string
  | `Docker_volume of string
  | `Obuilder_id of string
]

val image_name : ?tmp:bool -> S.id -> string
val container_name : S.id -> string

val docker_image : ?tmp:bool -> S.id -> [> `Docker_image of string ]
val docker_container : S.id -> [> `Docker_container of string ]

val result : string -> string -> string

val pull : ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
           [< `Docker_image of string ] -> unit Lwt.t
val export : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
             [< `Docker_container of string ] -> unit Lwt.t
val image : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
            string -> [< `Docker_image of string ] -> unit Lwt.t
val rm : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
         [ `Docker_container of string ] list -> unit Lwt.t
val rmi : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
          [ `Docker_image of string ] list -> unit Lwt.t
val tag : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
          ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
          [< `Docker_image of string ] -> [< `Docker_image of string ] ->
          unit Lwt.t
val commit : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
             [< `Docker_image of string ] ->
             [< `Docker_container of string ] ->
             [< `Docker_image of string ] ->
             unit Lwt.t
val volume : string list -> [< `Docker_volume of string ] ->
             string Lwt.t
val run : ?name:[< `Docker_container of string ] ->
          ?rm:bool ->
          string list ->
          [< `Docker_image of string ] ->
          string list ->
          unit Lwt.t
val run_result : pp:(Format.formatter -> unit) ->
                 ?name:[< `Docker_container of string ] ->
                 ?rm:bool ->
                 string list ->
                 [< `Docker_image of string ] ->
                 string list ->
                 (unit, [> `Msg of string ]) Result.result Lwt.t
val stop : pp:(Format.formatter -> unit) ->
           [< `Docker_container of string ] ->
           (unit, [> `Msg of string ]) Result.result Lwt.t

val exists : [< `Docker_container of string | `Docker_image of string
              | `Docker_volume of string ] ->
             (unit, [> `Msg of string ]) result Lwt.t

val obuilder_images : unit -> [ `Docker_image of string ] list Lwt.t
val obuilder_containers : unit -> [ `Docker_container of string ] list Lwt.t

(** Fetch (pull and extract) base images using Docker *)
module Extract : sig
  include S.FETCHER
end

(** Fetch (pull) base images using Docker *)
module Pull : sig
  include S.FETCHER
end
