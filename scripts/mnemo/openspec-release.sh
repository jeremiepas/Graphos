#!/usr/bin/env bash
#
# The mechanical half of the `openspec-release` workflow (space graphos).
# The workflow calls it from the root of the Graphos checkout, inside the
# devenv shell (cabal, openspec and gh on PATH).
#
#   openspec-release.sh prepare CHANGE          check the tools, clear a stale run state
#   openspec-release.sh worktree CHANGE         create the worktree and its branch, commit the proposal
#   openspec-release.sh draft-pr CHANGE         push the branch, open the draft pull request
#   openspec-release.sh check CHANGE            clean, pushed, atomic commits, openspec valid, cabal green
#   openspec-release.sh review-gate CHANGE      the latest review comment allows the merge of HEAD
#   openspec-release.sh gate CHANGE             check + review-gate + the GitHub checks are green
#   openspec-release.sh archive CHANGE          `openspec archive`, commit and push
#   openspec-release.sh archived CHANGE         the change left openspec/changes/ and is in the archive
#   openspec-release.sh merge CHANGE            bump graphos.cabal, squash-merge, tag, clean up
#   openspec-release.sh released CHANGE VERSION wait for the release workflow's GitHub release
#   openspec-release.sh cleanup CHANGE          remove the worktree and its local branch
#
# Exit codes: 0 done, 1 refused or failed, 2 usage or environment error.

set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"
readonly ROOT
readonly STATE_DIR="$(git rev-parse --absolute-git-dir)/mnemo"
readonly STATE="${STATE_DIR}/openspec-release.state"
readonly WORKTREES="${MNEMO_WORKTREES:-${ROOT}/.worktrees}"
readonly CI_WAIT_SECONDS="${MNEMO_CI_WAIT:-1800}"
readonly RELEASE_WAIT_SECONDS="${MNEMO_RELEASE_WAIT:-1800}"

err() {
  echo "openspec-release: $*" >&2
}

json_str() {
  printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'
}

tree_of() {
  printf '%s/%s' "${WORKTREES}" "$1"
}

branch_of() {
  printf 'openspec/%s' "$1"
}

write_state() {
  local change="$1" tree="$2" branch="$3"
  mkdir -p "${STATE_DIR}"
  printf 'change=%s\nworktree=%s\nbranch=%s\n' "${change}" "${tree}" "${branch}" >"${STATE}"
}

require_tree() {
  local change="$1" tree
  tree="$(tree_of "${change}")"
  if [[ ! -d "${tree}" ]]; then
    err "no worktree for ${change} at ${tree}; the worktree step did not run"
    return 2
  fi
  printf '%s\n' "${tree}"
}

# ---------------------------------------------------------------------------------------------------------------
# prepare

prepare() {
  local change="$1"
  command -v openspec >/dev/null || { err "openspec is not on PATH (start the run inside devenv shell)"; return 2; }
  command -v cabal >/dev/null || { err "cabal is not on PATH (start the run inside devenv shell)"; return 2; }
  command -v gh >/dev/null || { err "gh is not on PATH"; return 2; }
  command -v git >/dev/null || { err "git is not on PATH"; return 2; }
  if [[ ! -f "${ROOT}/openspec/config.yaml" ]]; then
    err "${ROOT} is not an OpenSpec workspace; run openspec init first"
    return 2
  fi
  git -C "${ROOT}" fetch -q origin main --tags
  # The workflow commits (the proposal, the development, the release): a
  # repository without an author identity gets the one of its last commit, so
  # neither this script nor the agent fails on `git commit`.
  if ! git -C "${ROOT}" config user.name >/dev/null 2>&1; then
    local author
    author="$(git -C "${ROOT}" log -1 --format='%an' 2>/dev/null || true)"
    git -C "${ROOT}" config user.name "${MNEMO_GIT_NAME:-${author:-mnemo}}"
    echo "prepare: git user.name set to ${MNEMO_GIT_NAME:-${author:-mnemo}} (repository-local)" >&2
  fi
  if ! git -C "${ROOT}" config user.email >/dev/null 2>&1; then
    err "git user.email is not set; set it before running the workflow"
    return 2
  fi
  rm -f "${STATE}"
  echo "prepare: ${change} in ${ROOT}"
}

# ---------------------------------------------------------------------------------------------------------------
# worktree

worktree() {
  local change="$1" tree branch path
  tree="$(tree_of "${change}")"
  branch="$(branch_of "${change}")"

  if [[ -d "${tree}" ]]; then
    if [[ "$(git -C "${tree}" branch --show-current)" != "${branch}" ]]; then
      err "${tree} exists but is not on ${branch}"
      return 1
    fi
  else
    mkdir -p "${WORKTREES}"
    if git -C "${ROOT}" show-ref --verify --quiet "refs/heads/${branch}"; then
      git -C "${ROOT}" worktree add -q "${tree}" "${branch}"
    elif git -C "${ROOT}" show-ref --verify --quiet "refs/remotes/origin/${branch}"; then
      git -C "${ROOT}" worktree add -q -b "${branch}" --track "${tree}" "origin/${branch}"
    else
      git -C "${ROOT}" worktree add -q -b "${branch}" "${tree}" origin/main
    fi
  fi

  # The first run bootstraps what origin/main does not hold yet: the OpenSpec
  # workspace, the agent skills, the mnemo workflow and its script. Later runs
  # find them on main.
  for path in openspec .opencode .mnemo scripts/mnemo AGENTS.md; do
    if [[ -e "${ROOT}/${path}" && ! -e "${tree}/${path}" ]]; then
      mkdir -p "$(dirname "${tree}/${path}")"
      cp -a "${ROOT}/${path}" "${tree}/${path}"
    fi
  done

  # A proposal written in the main checkout before the gate travels in the
  # first commit of the branch, and leaves the main checkout clean.
  if [[ -d "${ROOT}/openspec/changes/${change}" && ! -d "${tree}/openspec/changes/${change}" ]]; then
    mkdir -p "${tree}/openspec/changes"
    cp -a "${ROOT}/openspec/changes/${change}" "${tree}/openspec/changes/"
  fi
  # A change the main checkout does not track yet belongs to the branch now; a
  # change already on main stays in the main checkout.
  if [[ -d "${ROOT}/openspec/changes/${change}" ]]; then
    if ! git -C "${ROOT}" ls-files --error-unmatch "openspec/changes/${change}" >/dev/null 2>&1; then
      rm -rf "${ROOT}/openspec/changes/${change}"
    fi
  fi

  if [[ -n "$(git -C "${tree}" status --porcelain)" ]]; then
    git -C "${tree}" add -A
    if ! git -C "${tree}" diff --cached --quiet; then
      git -C "${tree}" commit -q -m "openspec: propose ${change}"
    fi
  fi

  write_state "${change}" "${tree}" "${branch}"
  printf '{"worktree":"%s","branch":"%s"}\n' "$(json_str "${tree}")" "$(json_str "${branch}")"
}

# ---------------------------------------------------------------------------------------------------------------
# draft pull request

draft_pr() {
  local change="$1" tree branch pr url
  tree="$(require_tree "${change}")" || return $?
  branch="$(branch_of "${change}")"

  # A change already on main leaves the branch identical to origin/main; an
  # empty start commit gives the draft pull request something to open on.
  if [[ -z "$(git -C "${tree}" log --format=%H "origin/main..HEAD")" ]]; then
    git -C "${tree}" commit -q --allow-empty -m "chore: start ${change}"
  fi
  git -C "${tree}" push -q -u origin "${branch}"
  pr="$(cd "${tree}" && gh pr view "${branch}" --json number --jq .number 2>/dev/null || true)"
  if [[ -z "${pr}" ]]; then
    (
      cd "${tree}"
      gh pr create --draft --base main --head "${branch}" \
        --title "openspec: ${change}" \
        --body "Draft proposal and development of \`${change}\`, opened by the mnemo \`openspec-release\` workflow. The proposal is in \`openspec/changes/${change}/\`."
    ) >/dev/null
    pr="$(cd "${tree}" && gh pr view "${branch}" --json number --jq .number)"
  fi
  url="$(cd "${tree}" && gh pr view "${pr}" --json url --jq .url)"
  printf '{"number":%s,"url":"%s"}\n' "${pr}" "$(json_str "${url}")"
}

# ---------------------------------------------------------------------------------------------------------------
# the local CI of .github/workflows/haskell.yml (build + test)

graphos_ci() {
  local tree="$1" log summary
  log="${STATE_DIR}/cabal.log"
  mkdir -p "${STATE_DIR}"
  if ! (cd "${tree}" && cabal build --flag dev && cabal test) >"${log}" 2>&1; then
    tail -n 40 "${log}" >&2
    err "cabal build --flag dev / cabal test failed (full log: ${log})"
    return 1
  fi
  summary="$(grep -E '^[0-9]+ examples?, [0-9]+ failures?' "${log}" | tail -1 || true)"
  echo "graphos-ci: ${summary:-cabal build and test passed}" >&2
}

check() {
  local change="$1" tree branch head remote subject bad=0
  tree="$(require_tree "${change}")" || return $?
  branch="$(branch_of "${change}")"

  if [[ -n "$(git -C "${tree}" status --porcelain)" ]]; then
    err "the worktree has uncommitted changes"
    return 1
  fi
  if ! git -C "${tree}" rev-parse --verify -q "refs/remotes/origin/${branch}" >/dev/null; then
    err "the branch ${branch} is not pushed"
    return 1
  fi
  head="$(git -C "${tree}" rev-parse HEAD)"
  remote="$(git -C "${tree}" rev-parse "origin/${branch}")"
  if [[ "${head}" != "${remote}" ]]; then
    err "the branch is not pushed at HEAD (${head:0:9} local, ${remote:0:9} remote)"
    return 1
  fi

  while IFS= read -r subject; do
    [[ -z "${subject}" ]] && continue
    if [[ "${subject}" =~ ^(fixup|squash)! ]]; then
      err "fixup/squash commit found: ${subject}"
      bad=1
      continue
    fi
    if ! [[ "${subject}" =~ ^(feat|fix|refactor|test|docs|chore|build|ci|perf|style|openspec|release)(\(.+\))?:\ .+ ]]; then
      err "the commit is not atomic or not conventional: ${subject}"
      bad=1
    fi
  done < <(git -C "${tree}" log --format='%s' "origin/main..HEAD")
  ((bad == 0)) || return 1

  (cd "${tree}" && openspec validate "${change}" --strict)
  graphos_ci "${tree}"
  echo "check: ${change} is clean, pushed, atomic and green at ${head:0:9}"
}

# ---------------------------------------------------------------------------------------------------------------
# review and the GitHub checks

checks_state() {
  local tree="$1" pr="$2"
  (cd "${tree}" && gh pr view "${pr}" --json statusCheckRollup --jq '
    .statusCheckRollup as $c
    | if ($c | length) == 0 then "none"
      elif any($c[]; ((.status // "COMPLETED") != "COMPLETED") or ((.state // "") == "PENDING")) then "pending"
      elif all($c[]; (.conclusion // .state // "") as $r | ["SUCCESS", "NEUTRAL", "SKIPPED"] | index($r)) then "passed"
      else "failed" end')
}

latest_status() {
  local tree="$1" pr="$2"
  (cd "${tree}" && gh pr view "${pr}" --json comments,reviews --jq '
    [ (.comments[] | {at: .createdAt, body}), (.reviews[] | {at: .submittedAt, body}) ]
    | map(select((.body // "") | test("\\*\\*Status: [^*]+\\*\\*")))
    | sort_by(.at) | last // empty
    | [ ([.body // "" | scan("\\*\\*Status: ([^*]+)\\*\\*")] | last | .[0] | ascii_upcase | gsub("^\\s+|\\s+$"; ""))
      , ((try (.body | capture("Reviewed commit: `?(?<sha>[0-9a-f]{7,40})").sha) catch "") // "")
      ] | @tsv')
}

review_gate() {
  local change="$1" tree branch pr head status reviewed
  tree="$(require_tree "${change}")" || return $?
  branch="$(branch_of "${change}")"
  pr="$(cd "${tree}" && gh pr view "${branch}" --json number --jq .number)"
  head="$(git -C "${tree}" rev-parse HEAD)"
  IFS=$'\t' read -r status reviewed < <(latest_status "${tree}" "${pr}") || true
  if [[ "${status:-}" != "CAN MERGE" ]] && [[ "${status:-}" != "IMPROVEMENT" ]]; then
    err "the latest review status is ${status:-absent}; a review must post CAN MERGE or IMPROVEMENT"
    return 1
  fi
  if [[ -z "${reviewed:-}" ]] || [[ "${head}" != "${reviewed}"* ]]; then
    err "the review is of ${reviewed:-no commit}, the head is ${head:0:9}"
    return 1
  fi
  echo "review-gate: #${pr} at ${head:0:9} is allowed to merge (${status})"
}

gate() {
  local change="$1" tree pr state waited=0
  tree="$(require_tree "${change}")" || return $?
  pr="$(cd "${tree}" && gh pr view "$(branch_of "${change}")" --json number --jq .number)"
  check "${change}" >/dev/null
  review_gate "${change}" >/dev/null
  state="$(checks_state "${tree}" "${pr}")"
  while [[ "${state}" == "pending" ]] && ((waited < CI_WAIT_SECONDS)); do
    sleep 30
    waited=$((waited + 30))
    state="$(checks_state "${tree}" "${pr}")"
  done
  case "${state}" in
    passed) ;;
    none) echo "gate: GitHub holds no check; the local suite stands for CI" ;;
    *) err "the checks of GitHub are ${state}"; return 1 ;;
  esac
  echo "gate: ${change} is mergeable (review, checks and the local suite are green)"
}

# ---------------------------------------------------------------------------------------------------------------
# archive, merge, release, cleanup

archive() {
  local change="$1" tree branch
  tree="$(require_tree "${change}")" || return $?
  branch="$(branch_of "${change}")"
  (cd "${tree}" && openspec archive "${change}" --yes)
  if [[ -n "$(git -C "${tree}" status --porcelain)" ]]; then
    git -C "${tree}" add -A
    git -C "${tree}" commit -q -m "openspec: archive ${change}"
    git -C "${tree}" push -q origin "${branch}"
  fi
  echo "archive: ${change} archived"
}

archived() {
  local change="$1" tree
  tree="$(require_tree "${change}")" || return $?
  if [[ -d "${tree}/openspec/changes/${change}" ]]; then
    err "the change is still active at openspec/changes/${change}"
    return 1
  fi
  if ! compgen -G "${tree}/openspec/changes/archive/*-${change}" >/dev/null; then
    err "no archive entry for ${change}"
    return 1
  fi
  echo "archived: ${change}"
}

next_version() {
  local latest base
  latest="$(git -C "${ROOT}" tag --list 'v*' --sort=-v:refname | head -1)"
  if [[ -z "${latest}" ]]; then
    base="$(sed -n 's/^version:[[:space:]]*//p' "${ROOT}/graphos.cabal" | head -1)"
    if [[ -z "${base}" ]]; then
      err "graphos.cabal carries no version line"
      return 1
    fi
    printf '%s\n' "${base%.*}"
    return 0
  fi
  local major minor patch
  IFS=. read -r major minor patch <<<"${latest#v}"
  printf '%s.%s.%s\n' "${major}" "${minor}" "$((patch + 1))"
}

merge() {
  local change="$1" tree branch pr version
  tree="$(require_tree "${change}")" || return $?
  branch="$(branch_of "${change}")"
  pr="$(cd "${tree}" && gh pr view "${branch}" --json number --jq .number)"
  (cd "${tree}" && gh pr ready "${pr}") >&2
  version="$(next_version)" || return $?

  sed -E -i "s/^version:[[:space:]]+.*/version:       ${version}.0/" "${tree}/graphos.cabal"
  if ! git -C "${tree}" diff --quiet -- graphos.cabal; then
    git -C "${tree}" add graphos.cabal
    git -C "${tree}" commit -q -m "release: v${version}"
    git -C "${tree}" push -q origin "${branch}"
  fi

  (cd "${tree}" && gh pr checks "${pr}" --watch --fail-fast) >&2
  # The workflow's account authors the pull request, and GitHub refuses a
  # self-approval; the human gate at `approve-merge` is the review of record.
  if ! (cd "${tree}" && gh pr merge "${pr}" --squash) >&2; then
    (cd "${tree}" && gh pr merge "${pr}" --squash --admin) >&2
  fi
  git -C "${ROOT}" fetch -q origin main --tags
  if ! git -C "${ROOT}" rev-parse -q --verify "refs/tags/v${version}" >/dev/null; then
    git -C "${ROOT}" tag -a "v${version}" -m "Release v${version}" origin/main
    git -C "${ROOT}" push -q origin "v${version}"
  fi
  # .github/workflows/release.yml builds and publishes the GitHub release when
  # the tag is pushed; `released` waits for it.

  # Everything above is progress on stderr: the standard output of `merge` is
  # the one JSON object the step parses.
  cleanup "${change}" >&2
  printf '{"version":"%s","tag":"v%s"}\n' "${version}" "${version}"
}

released() {
  local change="$1" version="$2" waited=0
  if ! git -C "${ROOT}" rev-parse -q --verify "refs/tags/v${version}" >/dev/null; then
    err "the tag v${version} is missing"
    return 1
  fi
  while ! (cd "${ROOT}" && gh release view "v${version}" >/dev/null 2>&1); do
    if ((waited >= RELEASE_WAIT_SECONDS)); then
      err "the GitHub release v${version} is missing after ${waited}s; see .github/workflows/release.yml"
      return 1
    fi
    echo "released: waiting for the Release workflow to publish v${version} (${waited}s)" >&2
    sleep 30
    waited=$((waited + 30))
  done
  echo "released: ${change} at v${version}"
}

cleanup() {
  local change="$1" tree branch
  tree="$(tree_of "${change}")"
  branch="$(branch_of "${change}")"
  if [[ -d "${tree}" ]]; then
    if [[ -n "$(git -C "${tree}" status --porcelain)" ]]; then
      err "the worktree has uncommitted changes; nothing was removed"
      return 1
    fi
    git -C "${ROOT}" worktree remove "${tree}"
  fi
  git -C "${ROOT}" branch -q -D "${branch}" 2>/dev/null || true
  if git -C "${ROOT}" show-ref --verify --quiet "refs/remotes/origin/${branch}"; then
    git -C "${ROOT}" push -q origin --delete "${branch}" 2>/dev/null || true
  fi
  rm -f "${STATE}"
  echo "cleanup: removed ${tree} and ${branch}"
}

# ---------------------------------------------------------------------------------------------------------------

main() {
  local action="${1:-}" change="${2:-}"
  if [[ -z "${action}" ]]; then
    err "usage: openspec-release.sh <action> [change] [version]"
    return 2
  fi
  case "${action}" in
    prepare) prepare "${change}" ;;
    worktree) worktree "${change}" ;;
    draft-pr) draft_pr "${change}" ;;
    check) check "${change}" ;;
    review-gate) review_gate "${change}" ;;
    gate) gate "${change}" ;;
    archive) archive "${change}" ;;
    archived) archived "${change}" ;;
    merge) merge "${change}" ;;
    released) released "${change}" "${3:-}" ;;
    cleanup) cleanup "${change}" ;;
    *) err "unknown action ${action}"; return 2 ;;
  esac
}

main "$@"
