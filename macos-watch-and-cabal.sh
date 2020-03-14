#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 ACTION"
  exit 1
fi

DIR=$(realpath $(dirname $0))

while true; do
  cabal $1

  fswatch --one-event --latency 2 --exclude '.*' --include '.*\.hs$' --include '.*\.cabal$' "$DIR"

  if [ $? -ne 0 ]; then
    exit $?
  fi
done
