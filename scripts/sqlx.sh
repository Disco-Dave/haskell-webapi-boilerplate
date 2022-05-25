#!/bin/bash

set -e


REPO_ROOT="$(dirname "$(realpath "$0")" | xargs dirname)"
eval "$("$REPO_ROOT/scripts/dotenv.sh" print)"

cd "$REPO_ROOT/database"

SQLX_IMAGE_NAME="boilerplate-sqlx:$BOILERPLATE_SQLX_VERSION"

if [[ "$(docker images -q "$SQLX_IMAGE_NAME" 2> /dev/null)" == "" ]]; then
  "$REPO_ROOT/scripts/build-sqlx.sh"
fi

ADDTIONAL_FLAGS=""

if [[ "$BOILERPLATE_MIGRATE_DATABASE_SOCKET" != "" ]]; then
  ADDTIONAL_FLAGS="--volume=$BOILERPLATE_MIGRATE_DATABASE_SOCKET:$BOILERPLATE_MIGRATE_DATABASE_SOCKET"
else
  ADDTIONAL_FLAGS="-it"
fi

docker run --rm \
  --network="host" \
  --volume="$REPO_ROOT/database/migrations:/usr/local/etc/sqlx/migrations" \
  --env BOILERPLATE_MIGRATE_DATABASE_URL="$BOILERPLATE_MIGRATE_DATABASE_URL" \
  --env BOILERPLATE_MIGRATE_WEBSERVER_USER_PASSWORD="$BOILERPLATE_MIGRATE_WEBSERVER_USER_PASSWORD" \
  --user "$(id --user):$(id --group)" \
  "$ADDTIONAL_FLAGS" \
  "$SQLX_IMAGE_NAME" \
  "$@"
