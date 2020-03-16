#!/bin/bash

if [ $# -e 0 ]; then
  echo "Usage: $0 ACTION ARGS"
  exit 1
fi

DIR=$(realpath $(dirname $0))

while true; do
  cabal "$@" &
  PID=$!
  echo "PROCESS: $PID"

  fswatch --one-event --latency 2 --exclude '.*' --include '.*\.hs$' --include '.*\.cabal$' "$DIR"
  echo "killing $PID"
  kill $PID
  while kill -0 $PID; do
    sleep 1
  done

  sleep 1
  EXITCODE=$?
  if [ $EXITCODE -ne 0 ]; then
    exit $EXITCODE
  fi
done
