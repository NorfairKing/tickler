#!/usr/bin/env bash
set -x

cd $HOME

pkill -f 'tickler-web-server serve'

set -e

export TICKLER_WEB_SERVER_PORT=8002
export TICKLER_WEB_SERVER_DEFAULT_INTRAY_URL="https://api.intray.cs-syd.eu"
export TICKLER_SERVER_PORT=8003
export TICKLER_SERVER_LOG_LEVEL=LevelDebug
export TICKLER_SERVER_WEB_HOST=localhost:${TICKLER_WEB_SERVER_PORT}
export TICKLER_SERVER_VERIFICATION_EMAIL_ADDRESS=verification@example.com
export TICKLER_SERVER_TRIGGERED_EMAIL_ADDRESS=triggered@example.com
export TICKLER_SERVER_LOOPERS_DEFAULT_ENABLED=False
export TICKLER_SERVER_LOOPER_STRIPE_FETCHER_ENABLED=True

tickler-web-server serve --persist-logins --admin admin $@ &
