let chflags ~dir =
  Os.sudo ["chflags"; "-R"; "0"; dir]
