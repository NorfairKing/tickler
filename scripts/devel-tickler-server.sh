#!/bin/bash

set -e
set -x

stack install :tickler-server --file-watch --exec='./scripts/restart-tickler-server.sh' --fast --ghc-options=-freverse-errors
