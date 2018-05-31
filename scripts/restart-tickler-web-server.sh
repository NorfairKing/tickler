#!/bin/bash
set -x

cd $HOME

killall tickler-web-server

set -e

export PORT=8000
export API_PORT=8001
export DEFAULT_INTRAY_URL="https://api.intray.cs-syd.eu"

tickler-web-server serve --persist-logins --admin admin $@ &
