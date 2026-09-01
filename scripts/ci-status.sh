#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

if [ $# -lt 1 ]; then
  echo "Usage: ci-status.sh <branch>"
  echo "Check CI status for a branch"
  exit 1
fi

BRANCH="$1"

echo "Checking CI status for branch: ${BRANCH}"
echo "---"

gh run list --branch "$BRANCH" --json status,conclusion,createdAt,updatedAt --jq ".[] | {status, conclusion, createdAt, updatedAt}"
