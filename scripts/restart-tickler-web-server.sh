#!/bin/bash
set -x

cd $HOME

pkill -f 'tickler-web-server serve'

set -e

export TICKLER_WEB_SERVER_PORT=8002
export TICKLER_WEB_SERVER_DEFAULT_INTRAY_URL="https://api.intray.cs-syd.eu"
export TICKLER_SERVER_PORT=8003
export TICKLER_SERVER_WEB_HOST=localhost:${TICKLER_WEB_SERVER_PORT}

tickler-web-server serve --persist-logins --admin admin $@ &
