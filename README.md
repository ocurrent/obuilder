# OBuilder

Status: **experimental**

OBuilder takes a build script (similar to a Dockerfile) and performs the steps in it in a sandboxed environment.

After each step, OBuild uses the snapshot feature of the filesystem (ZFS or Btrfs) to store the state of the build.
Repeating a build will reuse the cached results where possible.

OBuilder aims to be portable, although currently only Linux support is present.
On Linux, it uses `runc` to sandbox the build steps, but any system that can run a command safely in a chroot could be used.

OBuilder stores the log output of each build step.
This is useful for CI, where you may still want to see the output even if the result was cached from some other build.

As present, the initial base image is fetched from Docker Hub using `docker pull` and then snapshotted into the store.

## Usage

OBuilder is designed to be used as a component of a build scheduler such as [OCluster][].
However, there is also a command-line interface for testing.

To build `example.spec` (which builds OBuilder itself) using the ZFS pool `tank` to cache the build results:

    $ obuilder build -f example.spec . --store=zfs:tank

To use Btrfs directory `/mnt/btrfs` for the build cache, use `--store=btrfs:/mnt/btrfs`.

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

## The build specification language

The spec files are loosly based on the [Dockerfile][] format.
The main difference is that the format uses S-expressions rather than a custom format,
which should make it easier to generate and consume it automatically.

When performing a build, the user gives OBuilder a specification file (as described below),
and a source directory, containing files which may be copied into the image using `copy`.

At the moment, multi-stage builds are not supported, so a spec file is just a single stage, of the form:

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
 (shell COMMAND))
 
```

Examples:

```sexp
(run (shell "echo hello"))
```

```sexp
(run
 (cache (opam-archives (target /home/opam/.opam/download-cache)))
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

`NAME` must match the regexp `[A-Za-z][-._A-Za-z0-9]*`.

### copy

```sexp
(copy
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


[Dockerfile]: https://docs.docker.com/engine/reference/builder/
[OCluster]: https://github.com/ocurrent/ocluster
