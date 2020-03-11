#!/bin/bash

DIR=$(realpath $(dirname $0))

while true; do
  cabal test

  fswatch --one-event --latency 2 --exclude '.*' --include '.*\.hs$' --include '.*\.cabal$' "$DIR"

  if [ $? -ne 0 ]; then
    exit $?
  fi
done
