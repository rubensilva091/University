#!/bin/bash

set -eux

PUID=${PUID:-911}
PGID=${PGID:-911}

groupmod -o -g "$PGID" runner &> /dev/null
usermod -o -u "$PUID" runner &> /dev/null

chown -R runner:runner /app

exec gosu runner "$@"
