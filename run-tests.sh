#!/bin/sh

# Usage: ./run-tests.sh [pattern]
# If a pattern is provided, it will be forwarded to buttercup's --pattern option.

PATTERN="$1"

if [ -n "$PATTERN" ]; then
  UNDERCOVER_FORCE=true eask -g exec buttercup --pattern "$PATTERN" -L .
else
  UNDERCOVER_FORCE=true eask -g exec buttercup -L .
fi
