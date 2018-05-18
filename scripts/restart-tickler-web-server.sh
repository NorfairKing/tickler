#!/bin/bash
set -x

cd $HOME

killall tickler-web-server

set -e

export PORT=8000
export API_PORT=8001

tickler-web-server serve --persist-logins --admin admin &
