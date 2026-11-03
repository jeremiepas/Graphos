# AVI-1958: Merge gate Graphos PR #217 — Gate Decision

**Issue:** AVI-1958 Merge gate Graphos PR #217: AVI-1886: TB merge gate - formalize merge-gate status gate
**PR:** #217 (maps to branch `feature/AVI-1886-tb-merge-gate`)
**Issue:** AVI-1886 — TB merge gate - formalize merge-gate status gate
**PR state:** DIRTY (state=DIRTY, +365/-1)
**Gatekeeper:** merge-gatekeeper (agent f8893e71-6e9d-4daa-af65-e72cbf6aba77)
**Date:** 2026-11-02

## Gate Decision

The merge-gate status gate has been formalized via AVI-1887 change, adding `SHALL` requirements and `WHEN/THEN` scenarios to the `product-owner-graphos` spec. The gate encodes the `blockedBy` relationship between the merge-gate lane and the review lane.

### Change artifacts

- `openspec/changes/AVI-1887-tb-merge-gate-w-blockedBy/design.md` — Context, goals, non-goals, and decisions for the merge-gate status gate
- `openspec/changes/AVI-1887-tb-merge-gate-w-blockedBy/proposal.md` — Why, what, capabilities, impact, and modified capabilities
- `openspec/changes/AVI-1887-tb-merge-gate-w-blockedBy/tasks.md` — 4 tasks (1: proposal/design, 2: added requirements + scenarios, 3: validation, 4: commit/push/open PR)
- `openspec/changes/AVI-1887-tb-merge-gate-w-blockedBy/specs/product-owner-graphos/spec.md` — ADDED Requirements for merge-gate status gating

**Validation:** `openspec validate --changes` confirms `change/AVI-1887-tb-merge-gate-w-blockedBy` is purely additive, no existing requirements altered.

### Gate rule (from release-policy.md §2, Daily Graphos merge gate)

- PRs that are reviewed **and** mergeable get a merge-gate lane to merge-gatekeeper
- The merge-gatekeeper SHALL not process (merge) a merge-gate lane until its associated review lane has reached a `done` state
- The `blockedBy` relationship between the merge-gate lane and the review lane is the gate key
- As long as the review lane blocks the merge-gate lane, the merge-gatekeeper SHALL withhold merge approval

### Gate outcome

- **Merge gate CLOSED**: The merge gate remains blocked until the review lane AVI-1894 accepts the diff
- **Reference review lane**: AVI-1894 — the merge gate will remain closed until review lane AVI-1894 accepts the diff
- **Gate condition**: As long as the review lane associated with the merge-gate lane is not in a `done` state, the merge-gatekeeper SHALL not merge the merge-gate lane

### Scenarios

#### Scenario: review lane done → merge-gate lane may proceed

- **WHEN** a merge-gate lane's associated review lane has reached a `done` state
- **THEN** the merge-gate lane is eligible for processing by the merge-gatekeeper

#### Scenario: review lane not done → merge-gate lane stays blocked

- **WHEN** a merge-gate lane's associated review lane has not reached a `done` state
- **THEN** the merge-gatekeeper SHALL not merge the merge-gate lane; it stays in its current state

#### Scenario: partial/mixed review-lane state does not unblock

- **WHEN** the review lane associated with a merge-gate lane is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge

## Verification

- Command: `openspec validate --changes change/AVI-1887-tb-merge-gate-w-blockedBy` → passes (purely additive)
- Command: `git log --oneline feature/AVI-1886-tb-merge-gate..HEAD` → merge commit `07dd5d7` Merge branch 'feature/AVI-1887-tb-merge-gate-w-blockedBy' into feature/AVI-1886-tb-merge-gate
- Command: `git show --stat feature/AVI-1887-tb-merge-gate-w-blockedBy` → 4 files added (design.md, proposal.md, tasks.md, specs/product-owner-graphos/spec.md)
- Command: `git branch --contains feature/AVI-1886-tb-merge-gate` → branch contains the merge-gate status gate formalization

## Disposition

**Gate remains CLOSED** until review lane AVI-1894 accepts the diff. The merge-gatekeeper must wait for the review lane to reach the `done` state before proceeding with the merge.

**Next step**: The review lane AVI-1894 must accept the diff (i.e., the associated review issue must be resolved and marked `done`). Once the review lane is `done`, the merge-gatekeeper may proceed with merging PR #217 through the daily merge-gate routine (13:00 Europe/Paris).

**Explicit keep**: The branch `feature/AVI-1886-tb-merge-gate` is kept (not merged) and deferred pending review lane AVI-1894 acceptance. The merge gate will re-evaluate once the review lane accepts the diff.

**Verification command:** `git merge-base --is-ancestor 07dd5d7 main && echo "branch is merged into main" || echo "branch not merged into main"` → branch not yet merged into main (gate prevents merge)

## Dependencies

- **Depends on**: AVI-1887 — TB merge gate status gate formalization (complete)
- **Blocks**: None (merge gate is a gate on this issue, not a blocker for other work)
- **Blocked by**: Review lane AVI-1894 — must accept the diff before merge gate can open