#!/usr/bin/env bash

docker run -d -p 80:80 -e DB_PATH="/app/data/musiorder.db" -v "$PWD\data:/app/data" musiorder
