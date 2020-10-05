val dockerfile_of_spec : Spec.stage -> Dockerfile.t
(** [dockerfile_of_spec x] produces a Dockerfile that aims to be equivalent to [x].

    However, note that:

    - In "(copy (excludes ...) ...)" the excludes part is ignored. You will need to ensure
      you have a suitable ".dockerignore" file.
    - In "(run (cache ...) ...)" the caches are ignored. Regular Docker doesn't support this.
      It would be possible to switch to the extended BuildKit format in this case, but the
      normal use for [dockerfile_of_spec] is to let users duplicate a build easily, and
      it's easier if they don't need to set up BuildKit. *)
