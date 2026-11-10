#!/usr/bin/env bash
# post-github-review.sh — materialize a review verdict on GitHub via REST API.
#
# Usage: post-github-review.sh <pr-number> <event> [body]
#   event: APPROVE | REQUEST_CHANGES | COMMENT
#
# Posts POST /repos/{owner}/{repo}/pulls/{pr}/reviews with the given event, so
# required-approvals branch protection (and merge-gatekeeper) can observe the
# verdict. Prose-only Paperclip comments are never enough.
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "usage: $0 <pr-number> <APPROVE|REQUEST_CHANGES|COMMENT> [body]" >&2
  exit 2
fi

PR="$1"
EVENT_RAW="$2"
BODY="${3:-}"

REPO="${GITHUB_REPOSITORY:-jeremiepas/Graphos}"
EVENT="$(printf '%s' "$EVENT_RAW" | tr '[:lower:]' '[:upper:]')"

case "$EVENT" in
  APPROVE|REQUEST_CHANGES|COMMENT) ;;
  *)
    echo "invalid event '$EVENT_RAW' (want APPROVE, REQUEST_CHANGES, or COMMENT)" >&2
    exit 2
    ;;
esac

if ! [[ "$PR" =~ ^[0-9]+$ ]]; then
  echo "pr-number must be a positive integer" >&2
  exit 2
fi

# Build the JSON payload without putting secrets on argv beyond the review body.
payload="$(jq -n --arg event "$EVENT" --arg body "$BODY" \
  '{event: $event} + (if ($body|length) == 0 then {} else {body: $body} end)')"

echo "POST /repos/${REPO}/pulls/${PR}/reviews event=${EVENT}" >&2
gh api "repos/${REPO}/pulls/${PR}/reviews" --method POST --input - <<<"$payload"
