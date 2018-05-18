#!/bin/bash

set -e
set -x

./scripts/docker-build-tickler-web-server.sh
docker run \
  --rm \
  --publish 8000:80 \
  --volume /tmp/shared-tickler:/www/tickler-data \
  tickler-web-server:latest

