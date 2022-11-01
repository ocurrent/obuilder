### unreleased

- Add --fuse-path to allow selection of the path redirected by FUSE (@mtelvers #128, reviewed by @MisterDA )
- Pre-requisites for Windows support using docker for Windows (@MisterDA #116, reviewed by @tmcgilchrist)
- Add support for Docker/Windows spec (@MisterDA #117, reviewed by @tmcgilchrist)
- Depend on Lwt.5.6.1 for bugfixes (@MisterDA #108, reviewed by @tmcgilchrist)

- Add macOS support (@patricoferris #87, reviewed by @tmcgilchrist @talex5 @kit-ty-kate)
- Enable macOS tests only on macOS (@MisterDA #126, reviewed by @tmcgilchrist)
- Dune 3.0 generates empty intf for executables (@MisterDA #111, reviewed by @talex5)
- Fix warnings and CI failure (@MisterDA #110, reviewed by @talex5)

- Expose store root and cmdliner term with non-required store (@MisterDA #119, reviewed by @tmcgilchrist)
- Expose Rsync_store module (@MisterDA #114, reviewed by @talex5)
- Rsync hard-links to save space (@art-w #102, reviewed by @patricoferris)

### v0.4

- Use GNU tar format instead of UStar for `copy` operations (@TheLortex #82, reviewed @dra27).
  This enables copying from sources containing long file names (>100 characters).

- Add support for secrets (@TheLortex #63, reviewed by @talex5).
  The obuilder spec's `run` command supports a new `secrets` fields, which allows to temporarily
  mount secret files in an user-specified location. The sandbox build context has an additional
  `secrets` parameter to provide values for the requested keys.

- Limit permissions on temporary directories (@talex5 #67)

- Check Linux kernel version support for btrfs (@kit-ty-kate #68)

- Generalise obuilder sandbox, removing runc/linux specifc pieces and 
  making the S.SANDBOX interface more general
  (@patricoferris #58, reviewed by @talex5, @avsm, @MisterDA)
  
- Convert --fast-sync back to a flag (@talex5 #72)

- Support Fmt.cli and Logs.cli flags. (@MisterDA #74, reviewed by @talex5)
  For Fmt the new options are --color=always|never|auto
  For Log the new options are:
    -v, --verbose Increase verbosity
    --verbosity=LEVEL (absent=warning)
        Be more or less verbose. LEVEL must be one of quiet, error,
        warning, info or debug. Takes over -v.

- Minor cleanup changes (@talex5 #76)

- Fix deprecations in Fmt 0.8.10 (@tmcgilchrist #80)

- Remove travis-ci and replace with Github Actions (@MisterDA #84)

- Add RSync store backend for obuilder to support macOS builders (@patricoferris #88, reviewed @talex5)

- Fixes for ZFS tests in CI (@patricoferris #91)

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
