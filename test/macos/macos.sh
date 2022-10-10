#!/bin/sh
TMP_SCOREBOARD=$(mktemp -d 2>/dev/null || mktemp -d -t 'scoreboard')
TMP_RSYNC=$(mktemp -d 2>/dev/null || mktemp -d -t 'rsync')
dune exec -- obuilder build . -f ./specs/simple.macos.spec --uid=705 --store=rsync:$TMP_RSYNC --fallback=/ --scoreboard=$TMP_SCOREBOARD --no-fuse
dune exec -- obuilder build . -f ./specs/simple.macos.spec --uid=705 --store=rsync:$TMP_RSYNC --fallback=/ --scoreboard=$TMP_SCOREBOARD --no-fuse
rm -rf $TMP_SCOREBOARD $TMP_RSYNC