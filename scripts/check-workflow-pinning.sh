#!/usr/bin/env bash
# AVI-1521 — CI supply-chain policy for this repository:
#
#   1. every remote `uses:` action is pinned to a full 40-character commit SHA.
#      Tags and branches can be re-pointed by the action's owner at any time,
#      which would let unreviewed code run with this repo's credentials.
#   2. every workflow declares a top-level `permissions:` block, so the default
#      token scope is never inherited implicitly.
#
# Local actions (`uses: ./path`) are exempt: they ship with the repository.
# Run in CI by .github/workflows/ci-policy.yml, or locally:
#   scripts/check-workflow-pinning.sh
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

mapfile -t files < <(
  find "$root/.github/workflows" "$root/templates" \
    -type f \( -name '*.yml' -o -name '*.yaml' \) -path '*/workflows/*' 2>/dev/null | sort
)

if [ "${#files[@]}" -eq 0 ]; then
  echo "check-workflow-pinning: no workflow files found under $root" >&2
  exit 1
fi

fail=0

for file in "${files[@]}"; do
  rel=${file#"$root/"}

  if ! grep -qE '^permissions:' "$file"; then
    printf '::error file=%s::missing top-level "permissions:" block (declare least privilege, e.g. "contents: read")\n' "$rel"
    fail=1
  fi

  while IFS= read -r hit; do
    lineno=${hit%%:*}
    value=${hit#*:}
    value=${value#*uses:}
    value=${value%%#*}
    value=$(printf '%s' "$value" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//')
    value=${value//\"/}
    value=${value//\'/}

    case "$value" in
      ./*)
        continue
        ;;
      docker://*)
        printf '::error file=%s,line=%s::docker:// images cannot be SHA-pinned; use a pinned action or a local action\n' "$rel" "$lineno"
        fail=1
        continue
        ;;
    esac

    if [[ ! "$value" =~ @[0-9a-f]{40}$ ]]; then
      printf '::error file=%s,line=%s::uses: %s is not pinned to a full 40-character commit SHA\n' "$rel" "$lineno" "$value"
      fail=1
    fi
  done < <(grep -nE '^[[:space:]]*(-[[:space:]]+)?uses:' "$file" || true)
done

if [ "$fail" -ne 0 ]; then
  echo "" >&2
  echo "CI supply-chain policy violated (AVI-1521); see the script header for the rules." >&2
  exit 1
fi

echo "OK: ${#files[@]} workflow file(s) SHA-pinned, permissions declared"
