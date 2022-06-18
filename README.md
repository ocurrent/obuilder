# OBuilder

[![GitHub CI][github-shield]][github-ci]
[![docs][docs-shield]][docs]
[![OCaml-CI Build Status][ocaml-ci-shield]][ocaml-ci]


OBuilder takes a build script (similar to a Dockerfile) and performs the steps in it in a sandboxed environment.

After each step, OBuild uses the snapshot feature of the filesystem (ZFS or Btrfs) to store the state of the build. There is also an Rsync backend that copies the build state.
Repeating a build will reuse the cached results where possible.

OBuilder aims to be portable, although currently only Linux support is present.
On Linux, it uses `runc` to sandbox the build steps, but any system that can run a command safely in a chroot could be used.

OBuilder stores the log output of each build step.
This is useful for CI, where you may still want to see the output even if the result was cached from some other build.

As present, the initial base image is fetched from Docker Hub using `docker pull` and then snapshotted into the store.

## Usage

OBuilder is designed to be used as a component of a build scheduler such as [OCluster][].
However, there is also a command-line interface for testing.

To check that the system is working correctly, you can run a healthcheck.
This checks that Docker is running and then does a simple test build (pulling the `busybox` image if not already present):

    $ obuilder healthcheck --store=zfs:tank
    Healthcheck passed

To build `example.spec` (which builds OBuilder itself) using the ZFS pool `tank` to cache the build results:

    $ obuilder build -f example.spec . --store=zfs:tank

To use Btrfs directory `/mnt/btrfs` for the build cache, use `--store=btrfs:/mnt/btrfs` or specify a directory for Rsync to use `--store=rsync:/rsync`.

## Notes

Some operations (such as deleting btrfs snapshots) require root access.
OBuilder currently uses `sudo` as necessary for such operations.

You should only run one instance of the command-line client at a time with
a given store. OBuilder does support concurrent builds, but they must be
performed using a single builder object:

- If you try to perform an operation that is already being performed by another
  build, it will just attach to the existing build.

- The new client will get the logs so far, and then stream new log data as it
  arrives.

- If a client cancels, it just stops following the log.
  The operation itself is cancelled if all its clients cancel.

OBuilder calculates a digest of the input files to decide whether a `copy` step
needs to be repeated. However, if it decides to copy the file to the build sandbox,
it does not check the digest again. Also, it only checks that it is not following
symlinks during the initial scan. Therefore, you must not modify the input
files while a build is in progress.

Failed build steps are not cached.

Files and directories in the store may have owners and groups that only make sense
in the context of some container. The store should therefore be configured so
that other processes on the host (which might have the same IDs by coincidence)
cannot reach them, e.g. by `chmod go-rwx /path/to/store`.

Sync operations can be very slow, especially on btrfs. They're also
unnecessary, since if the computer crashes then we'll just discard the whole
build and start again. If you have runc version `v1.0.0-rc92` or later, you can
pass the `--fast-sync` option, which installs a seccomp filter that skips all
sync syscalls. However, if you attempt to use this with an earlier version of
runc then sync operations will instead fail with `EPERM`.

## The build specification language

The spec files are loosly based on the [Dockerfile][] format.
The main difference is that the format uses S-expressions rather than a custom format,
which should make it easier to generate and consume it automatically.

When performing a build, the user gives OBuilder a specification file (as described below),
and a source directory, containing files which may be copied into the image using `copy`.

```sexp
((from BASE) OP...)
```

Example:

```sexp
((from busybox@sha256:d366a4665ab44f0648d7a00ae3fae139d55e32f9712c67accd604bb55df9d05a)
 (shell /bin/sh -c)
 (run (shell "echo hello world")))
```

`BASE` identifies a Docker image, which will be fetched using `docker pull` and imported into the OBuilder cache.
OBuilder will not check for updates, so `BASE` should include a digest identifying the exact image, as shown above.

The operations are performed in order. Each operation gets a build context and a filesystem snapshot, and may produce
a new context and a new snapshot.
The initial filesystem snapshot is `BASE`. `run` and `copy` operations create new snapshots.

The initial context is supplied by the user (see [build.mli](lib/build.mli) for details).
By default:
- The environment is taken from the Docker configuration of `BASE`.
- The user is `(uid 0) (gid 0)`.
- The workdir is `/`.
- The shell is `/bin/bash -c`.

### Multi-stage builds

You can define nested builds and use the output from them in `copy` operations.
For example:

```sexp
((build dev
        ((from ocaml/opam:alpine-3.12-ocaml-4.11)
         (user (uid 1000) (gid 1000))
         (workdir /home/opam)
         (run (shell "echo 'print_endline {|Hello, world!|}' > main.ml"))
         (run (shell "opam exec -- ocamlopt -ccopt -static -o hello main.ml"))))
 (from alpine:3.12)
 (shell /bin/sh -c)
 (copy (from (build dev))
       (src /home/opam/hello)
       (dst /usr/local/bin/hello))
 (run (shell "hello")))
```

At the moment, the `(build ...)` items must appear before the `(from ...)` line.


### workdir

```sexp
(workdir DIR)
```

Example:

```sexp
(workdir /usr/local)
```

This operation sets the current working directory used for the following commands, until the next `workdir` operation.
If the path given is relative, it is combined with the previous setting.

### shell

```sexp
(shell ARG...)
```

Example:

```sexp
(shell /bin/bash -c)
```

This sets the shell used for future `(run (shell COMMAND))` operations.
The command run will be this list of arguments followed by the single argument `COMMAND`.

### run

```sexp
(run
 (cache CACHE...)?
 (network NETWORK...)?
 (secrets SECRET...)?
 (shell COMMAND))

```

Examples:

```sexp
(run (shell "echo hello"))
```

```sexp
(run
 (cache (opam-archives (target /home/opam/.opam/download-cache)))
 (network host)
 (secrets (password (target /secrets/password)))
 (shell "opam install utop"))
```

Runs the single argument `COMMAND` using the values in the current context (set by `workdir` and `shell`).

The `(cache CACHE...)` field can be used to mount one or more persistent caches for the command.
Each `CACHE` takes the form `(NAME (target PATH))`, where `NAME` uniquely identifies the cache to use
and `PATH` is the mount point within the container.

If the cache `NAME` does not yet exist then it is first created as an empty directory,
owned by the user in the build context.
A mutable copy of the cache is created for the command. When the command finishes (whether successful or not)
this copy becomes the new version of the cache, unless some other command updated the same cache first, in
which case this one is discarded.

The `(network NETWORK...)` field specifies which network(s) the container will be connected to.
`(network host)` is a special value which runs the container in the host's network namespace.
Otherwise, a fresh network namespace is created for the container, with interfaces for the given
networks (if any).

Currently, no other networks can be used, so the only options are `host` or an isolated private network.

The `(secrets SECRET...)` field can be used to request values for chosen keys, mounted as read-only files in
the image. Each `SECRET` entry is under the form `(ID (target PATH))`, where `ID` selects the secret, and
`PATH` is the location of the mounted secret file within the container.
The sandbox context API contains a `secrets` parameter to provide values to the runtime.
If a requested secret isn't provided with a value, the runtime fails.
With the command line interface `obuilder`, use the `--secret ID:PATH` option to provide the path of the file
containing the secret for `ID`.
When used with Docker, make sure to use the **buildkit** syntax, as only buildkit supports a `--secret` option.
(See https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information)

### copy

```sexp
(copy
 (from ...)?
 (src SRC...)
 (dst DST)
 (exclude EXCL...)?)
```

Examples:

```sexp
(copy
 (src .)
 (dst build/)
 (exclude .git _build))
```

```sexp
(copy
 (src platform.ml.linux)
 (dst platform.ml))
```

This copies files, directories and symlinks from the source directory (provided by the user when building) into
the image. If `DST` does not start with `/` then it is relative to the current workdir.

It has two forms:
- If `DST` ends with `/` then it copies each item in `SRC` to the directory `DST`.
- Otherwise, it copies the single item `SRC` as `DST`.

Files whose basenames are listed in `exclude` are ignored.
If `exclude` is not given, the empty list is used.
At present, glob patterns or full paths cannot be used here.

If `(from (build NAME))` is given then the source directory is the root directory of the named nested build.
Otherwise, it is the source directory provided by the user.

Notes:

- Unlike Docker's `COPY` operation, OBuilder copies the files using the current
  user and group IDs, as set with `(user ...)`.

- Both `SRC` and `DST` use `/` as the directory separator on all platforms.

- The copy is currently done by running `tar` inside the container to receive the files.
  Therefore, the filesystem must have a working `tar` binary.

### user

```sexp
(user (uid UID) (gid GID))
```

Example:

```sexp
(user (uid 1000) (gid 1000))
```

This updates the build context to set the user and group IDs used for the following `copy` and `run` commands.
Note that only numeric IDs are supported.

### env

```sexp
(env NAME VALUE)
```

Example:

```sexp
(env OPTIONS "-O2 -Wall")
```

Updates the build context so that the environment variable `NAME` has the value `VALUE` in future `run` operations.

## Convert to Dockerfile format

You can convert an OBuilder spec to a Dockerfile like this:

```shell
obuilder dockerfile -f example.spec > Dockerfile
```

The dockerfile should work the same way as the spec file, except for these limitations:

- In `(copy (excludes ...) ...)` the excludes part is ignored.
  You will need to ensure you have a suitable `.dockerignore` file instead.

- If you want to include caches or to use secrets, use `--buildkit` to output in the extended BuildKit syntax.

- All `(network ...)` fields are ignored, as Docker does not allow per-step control of
  networking.

## Licensing

OBuilder is licensed under the Apache License, Version 2.0.
See [LICENSE][] for the full license text.

[Dockerfile]: https://docs.docker.com/engine/reference/builder/
[OCluster]: https://github.com/ocurrent/ocluster
[LICENSE]: ./LICENSE

[github-shield]: https://github.com/ocurrent/obuilder/actions/workflows/main.yml/badge.svg
[github-ci]: https://github.com/ocurrent/obuilder/actions/workflows/main.yml

[docs-shield]:https://img.shields.io/badge/doc-online-blue.svg
[docs]: https://ocurrent.github.io/obuilder/

[ocaml-ci]: https://ci.ocamllabs.io/github/ocurrent/obuilder
[ocaml-ci-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focurrent%2Fobuilder%2Fmaster&logo=ocaml
