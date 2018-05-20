#!/bin/bash
set -x

cd $HOME

killall tickler-server

set -e

export PORT=8001

tickler-server serve --admin admin &
