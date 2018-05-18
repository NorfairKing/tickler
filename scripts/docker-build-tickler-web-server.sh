#!/bin/bash

set -e
set -x

stack build :tickler-web-server
binpathabs=$(stack path --local-install-root)/bin/tickler-web-server
mkdir -p deploy/dist
binpathrel="deploy/dist/tickler-web-server"
cp "${binpathabs}" "${binpathrel}"
docker build -t tickler-web-server deploy
