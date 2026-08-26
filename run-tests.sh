#!/bin/sh

# Usage: ./run-tests.sh [pattern]
# If a pattern is provided, it will be forwarded to buttercup's --pattern option.

set -eu

PATTERN="${1:-}"

if [ -n "$PATTERN" ]; then
  eask emacs --batch -L . -L tests -l buttercup -f buttercup-run-discover \
    -- --pattern "$PATTERN"
else
  eask emacs --batch -L . -L tests -l buttercup -f buttercup-run-discover
fi
