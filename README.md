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

OBuilder is intended to be used as a library, but at the moment there is just a command-line interface.

To build `example.spec` (which builds OBuilder itself) using the ZFS pool `tank` to cache the build results:

    $ obuilder build -f example.spec . --store=zfs:tank

To use Btrfs directory `/mnt/btrfs` for the build cache, use `--store=btrfs:/mnt/btrfs`.
