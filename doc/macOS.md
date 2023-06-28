# Experimental macOS Support

The macOS backend uses the "user" as the unit of abstraction for sandboxing. That is, for each build a new user is created.
This user inherits a home-directory from the store which may come from previous builds using the storage backend.

A macOS base image is really just a home directory and only requires one file to work, an `.obuilder_profile.sh`.
This is sourced every time a command is run and can be useful for setting paths for a given type of build.

For `spec`s that only need local, per-user access this is great but for quite a few builds we also need external system dependencies.
On macOS a vast majority of users do this using homebrew. Homebrew installs system dependencies into `/usr/local` using
pre-built binaries (a.k.a bottles). It can be placed elsewhere but will often then build from source.

For OBuilder this means our per-user builds will break if they are all fighting over the global homebrew, so instead OBuilder does the following:

 - On macOS we require a scoreboard directory in which we record a symlink that associates a users identifier (`uid`) to the same user's current home directory.
 - Another tool, [obuilderfs](https://github.com/patricoferris/obuilder-fs), provides a [FUSE][] filesystem that rewrites access to a given
   directory (here `/usr/local`) to where the symlink points to in the scoreboard directory.
 - A set of [scripts](https://github.com/patricoferris/macos-infra/tree/main/scripts) allows us to initialise homebrew in a base image and use
   this in the `(from <base-image>)` stage of our builds.

The goal of the experimental macOS backend was to see how far we get without introducing any virtualisation. It is not intended to be used like the
runc-Linux backend because it requires a lot more manual setup and care to be taken.

## Running the macOS backend with ZFS

In order to run the macOS backend to build a very simple `spec` (one that doesn't require the FUSE filesystem) you will need to:

 - Install [openZFSonOSX][] and it should be `2.0` or later (this is when automatic snapshot mounting was added).
 - Create the "base image" as a directory in `/Users` i.e `sudo mkdir /Users/empty` and add an empty `.obuilder_profile.sh` to that directory. Note this is if you are using the `User_temp` fetcher module as opposed to the `Docker` one.
 - To get a ZFS pool quickly without partitioning your disk you can run `mkfile 128m <some-path>` and then `sudo zpool create tank <some-path>`.
 - Create a dummy `obuilderfs` binary that is in your `PATH`, this can just be an empty shell-script.
 - From the root of this project run: `sudo dune exec -- obuilder macos . --store=zfs:/Volumes/tank -f example.macos.spec --uid=705 --fallback=/tmp --scoreboard=/tmp`.
   Because we are not running the FUSE filesystem the `fallback` and `scoreboard` directories should be somewhere you don't mind being written to but they won't
   actually be used.

## Running the macOS backend with rsync

This requires much less setup at the cost of it being very slow in comparison to snap-shotting filesystems. All you need to do is create a directory somewhere for the "snapshots" to be written and pass this as `--rsync:/path/to/dir`. Of course, you will need `rsync` installed.

## Docker images for macOS

As mentioned elsewhere, OBuilder also abstracts the initial `FETCHER` whose job it is, is to interpret the `(from <base-image>)` stage and setup the base image in a given directory. When run on Linux this uses docker and exports the image and untars the filesystem into the directory. The same can be done for macOS! You can create a directory locally and then use the following dockerfile

```Dockerfile
FROM scratch
COPY </path/to/macos/base/dir> /
CMD [ "/bin/bash" ]
```

Note the `CMD` is important for the internal `docker create` call to not fail.


[FUSE]: https://osxfuse.github.io/
[openZFSonOSX]: https://openzfsonosx.org/wiki/Downloads#2.1.0
