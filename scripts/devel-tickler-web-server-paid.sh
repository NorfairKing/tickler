#!/bin/bash

set -e
set -x

function check_set () {
  if [ -z ${!1+x} ]; then
    echo "$1 must be set"
    return 1
  fi
}
check_set TICKLER_SERVER_STRIPE_PLAN
check_set TICKLER_SERVER_STRIPE_SECRET_KEY
check_set TICKLER_SERVER_STRIPE_PUBLISHABLE_KEY

stack install :tickler-web-server --file-watch --exec="./scripts/restart-tickler-web-server.sh $@" --fast --ghc-options=-freverse-errors
