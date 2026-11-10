## Why

The Graphos release policy (§1-§2) requires that every PR carry a merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane. Today, the `blockedBy` relationship between the merge-gate lane for Graphos PR #215 (security/AVI-1487-eliminate-curl-credentials) and the review lane AVI-1896 is implicit — it is documented in the release policy but not encoded as an explicit, reviewable OpenSpec gate. This means:

- The `blockedBy` semantics are not formally verified or testable
- Inconsistent triage can occur: a merge-gate lane might be processed before its review lane AVI-1896 is complete, or vice versa
- The merge-gatekeeper has no deterministic gate to apply when deciding whether PR #215 is mergeable through the merge gate

A documented status gate makes the `blockedBy` relationship between the merge-gate lane and the review lane AVI-1896 explicit, deterministic, and reviewable via `openspec check`.

## What Changes

- Add an explicit **merge-gate status gate** for Graphos PR #215 to the Graphos release policy / `product-owner-graphos` spec: the merge-gate lane may only be processed (merged by merge-gatekeeper) when its associated review lane AVI-1896 is `done`. Otherwise, the merge-gate lane stays in its current state (typically `in_progress` or `blocked`).
- Require that the `blockedBy` relationship between the merge-gate lane and the review lane AVI-1896 be recorded durably on each Paperclip issue, so the gate decision is not held only in chat or governance memory.
- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios, matching the existing `product-owner-graphos` and gate-status-task style.

## Capabilities

### New Capabilities

- `product-owner-graphos` (extended): merge-gate status gating keyed on review-lane AVI-1896 resolution.
- Paperclip lane `blockedBy` relationship: the merge-gate lane `blockedBy` the review lane AVI-1896 is durably recorded and enforceable.

### Modified Capabilities

- The `product-owner-graphos` spec gains additive requirements; no existing requirement is altered.
- The release policy `docs/release-policy.md` gains explicit gate wording; the underlying Paperclip lane machinery is unchanged.

## Impact

- **Specs**: purely additive to `openspec/changes/AVI-1960-merge-gate-pr-215-security-AVI-1487/specs/product-owner-graphos/spec.md`, delta onto the existing `product-owner-graphos` spec.
- **Code**: none — this documents merge-gatekeeper workflow; it does not touch Haskell sources, the cabal file, or OpenSpec core.
- **Process**: shapes how the merge-gatekeeper performs triage on each wake, converting an implicit `blockedBy` convention into a repeatable, recorded rule.