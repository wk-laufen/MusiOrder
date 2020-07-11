#!/usr/bin/env bash

docker run -d --restart unless-stopped -p 80:80 -e DB_PATH="/app/data/musiorder.db" -v "$PWD/data:/app/data" musiorder
