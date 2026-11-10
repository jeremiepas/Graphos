## Why

The Graphos release policy (§1, §2) requires that every PR carry a merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane. The daily Graphos merge gate routine (13:00 Europe/Paris) processes PRs that are reviewed and mergeable. However, for PR #208 (pr104-head), the `blockedBy` relationship between the merge-gate lane and review lane AVI-1903 is not codified as an explicit gate — it is only mentioned in the issue AVI-1967 description. This creates ambiguity about when the merge-gatekeeper may proceed to merge PR #208 through the merge gate.

Since the parent issue AVI-1876 (Daily Graphos PR intake watchdog) ensures that every open PR carries a live review lane and creates missing lanes same day, the merge gate `blockedBy` relationship with AVI-1903 is the natural formalization: gating the merge-gate lane's processing on review lane AVI-1903 being `done`.

## What Changes

- Add an explicit **merge-gate lane gated on review-lane AVI-1903 resolution**: a merge-gate lane may only be processed (merged by merge-gatekeeper) when its associated review lane AVI-1903 is `done`. Otherwise, the merge-gate lane stays in its current state (typically `in_progress` or `blocked`).
- Require that the `blockedBy` relationship between merge-gate lane and review lane AVI-1903 be recorded durably on each Paperclip issue, so the gate decision is not held only in prose.
- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios, matching the existing `product-owner-graphos` and gate-status-task style.
- Reference PR #208 (pr104-head) as the specific PR affected by this gate.

## Capabilities

### New Capabilities

- `product-owner-graphos` (extended): merge-gate lane gated on review-lane AVI-1903 resolution, specifically for PR #208 (pr104-head).
- Paperclip lane `blockedBy` relationship: the merge-gate lane `blockedBy` review lane AVI-1903 is durably recorded and enforceable for this PR.

### Modified Capabilities

- The `product-owner-graphos` spec gains additive requirements; no existing requirement is altered.
- The issue AVI-1967 documents the gate explicitly; the underlying Paperclip lane machinery is unchanged.

## Impact

- **Specs**: purely additive to `openspec/changes/AVI-1967-merge-gate-pr208-blockedBy-avi1903/specs/product-owner-graphos/spec.md`, delta onto the existing `product-owner-graphos` spec.
- **Code**: none — this documents merge-gatekeeper workflow; it does not touch Haskell sources, the cabal file, or OpenSpec core.
- **Process**: shapes how the merge-gatekeeper triages PR #208, converting an implicit `blockedBy` convention into a repeatable, recorded rule.