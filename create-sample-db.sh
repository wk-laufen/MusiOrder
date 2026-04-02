#!/bin/bash

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
DB_PATH="${SCRIPT_DIR}/data/musiorder.db"

sqlite3 "$DB_PATH" < "${SCRIPT_DIR}/db-schema.sql"
sqlite3 "$DB_PATH" < "${SCRIPT_DIR}/db-sample.sql"

echo "Sample database created at: $DB_PATH"
