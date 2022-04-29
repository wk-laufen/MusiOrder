#!/usr/bin/env bash

VERSION = $1
docker run --name musiorder-$VERSION -d --restart unless-stopped -p 80:80 -e DB_PATH="/app/data/musiorder.db" -v "$PWD/data:/app/data" johannesegger/musiorder:$VERSION
