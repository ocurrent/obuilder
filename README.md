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
it does not check the digest again. Therefore, you must not modify the input
files while a build is in progress.

Failed build steps are not cached.

[OCluster]: https://github.com/ocurrent/ocluster
