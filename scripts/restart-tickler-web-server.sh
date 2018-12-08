#!/bin/bash
set -x

cd $HOME

pkill -f 'tickler-web-server serve'

set -e

export WEB_PORT=8002
export WEB_HOST=localhost:${WEB_PORT}
export API_PORT=8003
export DEFAULT_INTRAY_URL="https://api.intray.cs-syd.eu"

tickler-web-server serve --persist-logins --admin admin $@ &
