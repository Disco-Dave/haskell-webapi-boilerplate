#!/bin/bash

set -e

REPO_ROOT="$(dirname "$(realpath "$0")" | xargs dirname)"

cd "$REPO_ROOT"

function print_file {
  if [[ -f "$1" ]]; then
    # Source: https://gist.github.com/judy2k/7656bfe3b322d669ef75364a46327836?permalink_comment_id=3666050#gistcomment-3666050
    grep -v '^#' "$1" | sed -E 's|^([^\=]+)=(.*)$|export \1=${\1:-\2}|g' | xargs -L 1
  fi
}

function print_all {
  for file in .env.*; do
    print_file "$file"
  done

  print_file ".env"
}

if [[ "$1" == "print" ]]; then
  print_all
else
  eval "$(print_all)"

  "$@"
fi
