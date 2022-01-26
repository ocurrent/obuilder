#!/bin/sh
mkdir -p /tmp/scoreboard
../../main.exe build . -f ./specs/simple.macos.spec --uid=705 --store=rsync:/tmp/rsync --fallback=/ --scoreboard=/tmp/scoreboard --no-fuse
../../main.exe build . -f ./specs/simple.macos.spec --uid=705 --store=rsync:/tmp/rsync --fallback=/ --scoreboard=/tmp/scoreboard --no-fuse
sudo rm -rf /tmp/rsync