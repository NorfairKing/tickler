#!/bin/bash
set -x

cd $HOME

killall tickler-server

set -e

export WEB_HOST=localhost:8002
export API_PORT=8001

tickler-server serve --admin admin $@ &
