#!/bin/bash

if [ $# -e 0 ]; then
  echo "Usage: $0 ACTION ARGS"
  exit 1
fi

DIR=$(realpath $(dirname $0))

while true; do
  cabal "$@" &
  CHILD_PID=$!

  fswatch --one-event --latency 2 --exclude '.*' --include '.*\.hs$' --include '.*\.cabal$' "$DIR"
  kill -9 $CHILD_PID
  fg

  sleep 1
  EXITCODE=$?
  if [ $EXITCODE -ne 0 ]; then
    exit $EXITCODE
  fi
done
