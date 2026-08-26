#!/bin/sh

# Usage: ./run-coverage.sh [pattern]
# Run Buttercup with Undercover enabled, then print a coverage summary.

set -eu

PATTERN="${1:-}"
REPORT_FILE="coverage/.resultset.json"

mkdir -p "$(dirname "$REPORT_FILE")"
rm -f "$REPORT_FILE"

if [ -n "$PATTERN" ]; then
  UNDERCOVER_FORCE=true ./run-tests.sh "$PATTERN"
else
  UNDERCOVER_FORCE=true ./run-tests.sh
fi

emacs --batch -Q -L tests -l eds-coverage-summary \
  --eval "(eds-coverage-summary \"$REPORT_FILE\")"
