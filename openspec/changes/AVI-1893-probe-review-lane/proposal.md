## Why

The Graphos release policy (§1-§2) requires that every PR carry a live review lane. Today, the review lane's completion is tracked independently of the PROBE check (e.g., intentgraph non-triviality probe or similar validation). The PROBE verification is a prerequisite step that must complete before a PR can be considered fully reviewed. Without explicit gating, the review lane can be marked `done` before PROBE completes, or reviewers may be unsure whether PROBE needs to be run as part of the review process.

A documented status gate makes the `blockedBy` relationship between the review lane and the PROBE check explicit, deterministic, and reviewable via `openspec check`.

## What Changes

- Add an explicit **PROBE gate** to the review lane: a review lane may only reach a `done` state when its associated PROBE check has reached a `done` state. Otherwise, the review lane stays in its current state (typically `in_progress` or `blocked`).
- Require that the `blockedBy` relationship between review lane and PROBE check be recorded durably on each Paperclip issue, so the gate decision is not held only in chat or governance memory.
- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios, matching the existing `product-owner-graphos` and gate-status-task style.

## Capabilities

### New Capabilities

- `product-owner-graphos` (extended): review lane gated on PROBE-check resolution.
- Paperclip lane `blockedBy` relationship: the review lane `blockedBy` the PROBE check is durably recorded and enforceable.

### Modified Capabilities

- The `product-owner-graphos` spec gains additive requirements; no existing requirement is altered.
- The release policy `docs/release-policy.md` gains explicit gate wording; the underlying Paperclip lane machinery is unchanged.

## Impact

- **Specs**: purely additive to `openspec/changes/AVI-1893-probe-review-lane/specs/product-owner-graphos/spec.md`, delta onto the existing `product-owner-graphos` spec.
- **Code**: none — this documents review workflow; it does not touch Haskell sources, the cabal file, or OpenSpec core.
- **Process**: shapes how reviewers and the merge-gatekeeper triage review completion, converting an implicit PROBE convention into a repeatable, recorded rule.