#!/bin/bash

set -e

export DATABASE_URL="$BOILERPLATE_MIGRATE_DATABASE_URL"

until psql "$BOILERPLATE_MIGRATE_DATABASE_URL" -tAc '\q'; do
  echo "Postgres is unavailable - sleeping"
  sleep 1
done

if ! psql "$BOILERPLATE_MIGRATE_DATABASE_URL" -tAc "SELECT 1 from pg_roles WHERE rolname='webserver';" | grep -q 1 ; then
    echo "Adding webserver user"
    psql "$BOILERPLATE_MIGRATE_DATABASE_URL" -c "CREATE USER webserver WITH PASSWORD '$BOILERPLATE_MIGRATE_WEBSERVER_USER_PASSWORD';"
fi

"$@"
