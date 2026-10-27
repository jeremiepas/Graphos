#!/usr/bin/env bash
# Thin wrapper so `paperclip/scripts/status.sh` works from any cwd.
set -euo pipefail
here="$(cd "$(dirname "$0")/.." && pwd)"
exec node "$here/status.mjs" "$@"
