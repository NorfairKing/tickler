#!/usr/bin/env bash
set -x

cd $HOME

killall tickler-server

set -e

export TICKLER_SERVER_WEB_HOST=localhost:8002
export TICKLER_SERVER_PORT=8001
export TICKLER_SERVER_VERIFICATION_EMAIL_ADDRESS=verification@example.com
export TICKLER_SERVER_TRIGGERED_EMAIL_ADDRESS=triggered@example.com
export TICKLER_SERVER_LOOPERS_DEFAULT_ENABLED=False


tickler-server serve $@ &
