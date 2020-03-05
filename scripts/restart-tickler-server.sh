#!/bin/bash
set -x

cd $HOME

killall tickler-server

set -e

export TICKLER_SERVER_WEB_HOST=localhost:8002
export TICKLER_SERVER_API_PORT=8001

tickler-server serve --admin admin $@ &
