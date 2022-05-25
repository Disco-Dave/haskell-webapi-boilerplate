#!/bin/bash

set -e

REPO_ROOT="$(dirname "$(realpath "$0")" | xargs dirname)"

eval "$("$REPO_ROOT/scripts/dotenv.sh" print)"

cd "$REPO_ROOT/database"

docker build \
  -t "boilerplate-sqlx:$BOILERPLATE_SQLX_VERSION" \
  --build-arg SQLX_VERSION="$BOILERPLATE_SQLX_VERSION" \
  .
