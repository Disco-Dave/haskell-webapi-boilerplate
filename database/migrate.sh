#!/bin/bash

set -e

script_path=$(realpath "$0")
database_directory=$(dirname "$script_path")
cd "$database_directory"

export DATABASE_URL="$BOILERPLATE_MIGRATE_DATABASE_URL"

function run_script() {
  psql "$BOILERPLATE_MIGRATE_DATABASE_URL" -q --single-transaction "$@"
}

until psql "$BOILERPLATE_MIGRATE_DATABASE_URL" -tAc '\q'; do
  echo "Postgres is unavailable - sleeping"
  sleep 1
done

sqlx database setup
run_script -f ./post-scripts/users/webserver.sql --set password="$BOILERPLATE_MIGRATE_WEBSERVER_USER_PASSWORD"
run_script -f ./post-scripts/users/reporting.sql --set password="$BOILERPLATE_MIGRATE_REPORTING_USER_PASSWORD"
