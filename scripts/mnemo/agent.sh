#!/usr/bin/env bash
#
# The delegate target of the graphos space (`opencode-deepseek`).
#
# mnemo gives the card on standard input and starts a delegate in the directory
# it was itself started from (the main checkout). The proposal step runs there;
# the development and review steps must run in the worktree recorded by
# `openspec-release.sh worktree`, so this wrapper enters it when it exists.
#
# The model defaults to the one the graphos configuration names; override it
# with MNEMO_GRAPHOS_MODEL.

set -euo pipefail

STATE="$(git rev-parse --absolute-git-dir)/mnemo/openspec-release.state"

if [[ -f "${STATE}" ]]; then
  TREE="$(sed -n 's/^worktree=//p' "${STATE}" | head -1)"
  if [[ -n "${TREE}" && -d "${TREE}" ]]; then
    cd "${TREE}"
  fi
fi

exec opencode run --auto --model "${MNEMO_GRAPHOS_MODEL:-litellm/montech/Ornith-1.5-9B-Q8_0}"
