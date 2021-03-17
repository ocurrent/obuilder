### unreleased

- Add support for secrets (@TheLortex #63, reviewed by @talex5).
  The obuilder spec's `run` command supports a new `secrets` fields, which allows to temporarily
  mount secret files in an user-specified location. The sandbox build context has an additional
  `secrets` parameter to provide values for the requested keys.

### v0.3

Security fix:

- `resolv.conf` file should be mounted read-only.

Other changes:

- Make `Os` and `Db` modules private. Move the `env` type to `Config`, as that is used externally.

- Fix license. It was copy-pasted from OCurrent, and still mentioned that project's `lib_ansi` library.

- Require obuilder-spec package to be same version.

### v0.2

- Add support for nested / multi-stage builds (@talex5 #48 #49).  
  This allows you to use a large build environment to create a binary and then
  copy that into a smaller runtime environment. It's also useful to get better caching
  if two things can change independently (e.g. you want to build your software and also
  a linting tool, and be able to update either without rebuilding the other).

- Add healthcheck feature (@talex5 #52).  
  - Checks that Docker is running.
  - Does a test build using busybox.

- Clean up left-over runc containers on restart (@talex5 #53).  
  If btrfs crashes and makes the filesystem read-only then after rebooting there will be stale runc directories.
  New jobs with the same IDs would then fail.

- Remove dependency on dockerfile (@talex5 #51).  
  This also allows us more control over the formatting
  (e.g. putting a blank line between stages in multi-stage builds).

- Record log output from docker pull (@talex5 #46).  
  Otherwise, it's not obvious why we've stopped at a pull step, or what is happening.

- Improve formatting of OBuilder specs (@talex5 #45).

- Use seccomp policy to avoid necessary sync operations (@talex5 #44).  
  Sync operations are really slow on btrfs. They're also pointless,
  since if the computer crashes while we're doing a build then we'll just throw it away and start again anyway.
  Use a seccomp policy that causes all sync operations to "fail", with errno 0 ("success").
  On my machine, this reduces the time to `apt-get install -y shared-mime-info` from 18.5s to 4.7s.
  Use `--fast-sync` to enable to new behaviour (it requires runc 1.0.0-rc92).

- Use a mutex to avoid concurrent btrfs operations (@talex5 #43).  
  Btrfs deadlocks enough as it is. Don't stress it further by trying to do two things at once.

Internal changes:

- Improve handling of file redirections (@talex5 #46).  
  Instead of making the caller do all the work of closing the file descriptors safely, add an `FD_move_safely` mode.

- Travis tests: ensure apt cache is up-to-date (@talex5 #50).

### v0.1

Initial release.
