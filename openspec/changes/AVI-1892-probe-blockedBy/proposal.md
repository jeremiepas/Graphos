## Why

The Graphos release policy (§1-§2) requires that every PR carry a merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane. Today, the `blockedBy` relationship between the merge-gate lane and the review lane is implicit — it is documented in the release policy but not encoded as an explicit, reviewable OpenSpec gate. This creates ambiguity about when the merge-gatekeeper may proceed to merge a PR through the merge gate.

Beyond the review-to-merge-gate `blockedBy` relationship, there is a need to also formalize a `blockedBy` relationship involving the **PROBE** check. The PROBE verification (e.g., intentgraph non-triviality probe or similar validation) is a prerequisite step that must complete before the merge gate can process a PR. Without explicit gating, the PROBE status can be overlooked during triage, leading to PRs being merged before PROBE completes, or the merge-gatekeeper being unsure whether PROBE needs to run.

A documented status gate makes the `blockedBy` relationship between the merge-gate lane and the PROBE check explicit, deterministic, and reviewable via `openspec check`.

## What Changes

- Add an explicit **PROBE gate** to the Graphos release policy / `product-owner-graphos` spec: a merge-gate lane may only be processed (merged by merge-gatekeeper) when its associated PROBE check has reached a `done` state. Otherwise, the merge-gate lane stays in its current state (typically `in_progress` or `blocked`).
- Require that the `blockedBy` relationship between merge-gate lane and PROBE check be recorded durably on each Paperclip issue, so the gate decision is not held only in chat or governance memory.
- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios, matching the existing `product-owner-graphos` and gate-status-task style.

## Capabilities

### New Capabilities

- `product-owner-graphos` (extended): merge-gate status gating keyed on PROBE-check resolution.
- Paperclip lane `blockedBy` relationship: the merge-gate lane `blockedBy` the PROBE check is durably recorded and enforceable.

### Modified Capabilities

- The `product-owner-graphos` spec gains additive requirements; no existing requirement is altered.
- The release policy `docs/release-policy.md` gains explicit gate wording; the underlying Paperclip lane machinery is unchanged.

## Impact

- **Specs**: purely additive to `openspec/changes/AVI-1892-probe-blockedBy/specs/product-owner-graphos/spec.md`, delta onto the existing `product-owner-graphos` spec.
- **Code**: none — this documents merge-gatekeeper workflow; it does not touch Haskell sources, the cabal file, or OpenSpec core.
- **Process**: shapes how the merge-gatekeeper performs triage on each wake, converting an implicit PROBE convention into a repeatable, recorded rule.