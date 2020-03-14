#!/bin/bash

DIR=$(realpath $(dirname $0))

$DIR/macos-watch-and-cabal.sh build
