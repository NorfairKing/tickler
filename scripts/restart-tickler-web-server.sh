#!/usr/bin/env bash
set -x

cd tickler-web-server

killall tickler-server || true
killall tickler-web-server || true

set -e

export TICKLER_SERVER_PORT=8000
export TICKLER_WEB_SERVER_PORT=8080
export TICKLER_SERVER_LOG_LEVEL=LevelDebug
export TICKLER_SERVER_WEB_HOST=http://localhost:${TICKLER_WEB_SERVER_PORT}
export TICKLER_SERVER_VERIFICATION_EMAIL_ADDRESS=verification@example.com
export TICKLER_SERVER_TRIGGERED_EMAIL_ADDRESS=triggered@example.com
export TICKLER_SERVER_LOOPERS_DEFAULT_ENABLED=False
export TICKLER_SERVER_LOOPER_TRIGGERER_ENABLED=True
export TICKLER_WEB_SERVER_API_URL=http://localhost:${TICKLER_SERVER_PORT}
export TICKLER_WEB_SERVER_DEFAULT_INTRAY_URL="https://api.intray.cs-syd.eu"

tickler-server --admin admin &
sleep 0.1
tickler-web-server --persist-logins $@ &
