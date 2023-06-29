val dockerfile_of_spec : buildkit:bool -> os:[`Unix | `Windows] -> Spec.t -> string
(** [dockerfile_of_spec ~buildkit ~os x] produces a Dockerfile
   that aims to be equivalent to [x].

    However, note that:

    - In "(copy (excludes …) …)" the excludes part is ignored. You will need to ensure
      you have a suitable ".dockerignore" file.
    - The conversion is not robust against malicious input, as the escaping rules are unclear.

    @param buildkit If true, the extended BuildKit syntax is used to
      support caches.  If false, caches are ignored. BuildKit syntax
      isn't supported on Windows.
    @param os Use UNIX or Windows syntax and idiosyncrasies. *)
