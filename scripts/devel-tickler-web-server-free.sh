#!/usr/bin/env bash

set -e
set -x

stack install :tickler-web-server --file-watch --exec="./scripts/restart-tickler-web-server.sh $@" --fast --ghc-options=-freverse-errors
