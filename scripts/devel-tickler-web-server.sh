#!/usr/bin/env bash

set -e
set -x

export DEVELOPMENT=True

stack install :tickler-web-server \
  --file-watch --watch-all \
  --no-nix-pure \
  --exec="./scripts/restart-tickler-web-server.sh $@" --fast --ghc-options=-freverse-errors
