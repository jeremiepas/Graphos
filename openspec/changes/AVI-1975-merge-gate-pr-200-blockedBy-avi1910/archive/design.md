## Context

The Graphos release policy (§1, §2) specifies that every PR must carry a merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane. The daily Graphos merge gate routine (13:00 Europe/Paris) processes PRs that are reviewed and mergeable. However, the `blockedBy` relationship between the merge-gate lane for Graphos PR #200 (`feature/AVI-1731-definition-of-done`) and the review lane **AVI-1911** is not codified as an explicit gate — it is only mentioned in prose in the release policy. This creates ambiguity about when the merge-gatekeeper may proceed to merge PR #200 through the merge gate.

Since the parent issue AVI-1876 (Daily Graphos PR intake watchdog) ensures that every open PR carries a live review lane and creates missing lanes same day, the merge gate `blockedBy` relationship is the natural next formalization: gating the merge-gate lane's processing on the review lane AVI-1911 being `done`.

## Goals / Non-Goals

**Goals:**
- Gate the merge-gate lane's processing on review-lane AVI-1911 resolution: `done` → proceed to merge; any other state → stay in current lane state (do not merge).
- Require the `blockedBy` relationship between merge-gate lane and review lane AVI-1911 be recorded durably on each Paperclip issue, so the gate decision is not held only in prose.
- Keep the change purely additive to the existing `product-owner-graphos` spec and release policy.
- No Haskell build/test is required; the change is spec-only.

**Non-Goals:**
- Changing Paperclip's own status-machine semantics; this documents merge-gatekeeper behavior, it does not modify how the board stores statuses.
- Altering existing requirements in the `product-owner-graphos` spec or any other live spec.
- Writing or reviewing Haskell code, or changing the build/test pipeline.
- Deciding which statuses exist on the board (that is a company/board decision).

## Decisions

- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios in the `product-owner-graphos` spec (the canonical home for PO/merge-gate behavior), rather than scattering prose across chat comments.
- All-or-nothing gating: a review lane AVI-1911 that is not `done` does NOT allow the merge-gate lane to proceed.
- Record every gate outcome durably (comment or document) so partial or no-change decisions survive across heartbeats.
- **All-or-nothing** — partial/mixed review-lane state does not unblock.

## Verification

- `openspec check` (or equivalent validation) passes for the new change: every requirement uses `SHALL`/`MUST` and every scenario uses exactly four `####` headers with `WHEN`/`THEN`.
- Manual read-through confirms the gate logic matches the AVI-1975 objective and is consistent with the existing `product-owner-graphos` requirements (no contradictions, no modified requirements).
- No Haskell build/test is required; the change is spec-only.

## Summary

The merge-gate status gate for PR #200 (`feature/AVI 1731 definition of done`) now exists as an explicit **SHALL** requirement in `openspec/changes/AVI-1975-merge-gate-pr-200-blockedBy-avi1910/specs/product-owner-graphos/spec.md`, with four WHEN/THEN scenarios and a re-evaluation each daily run (13:00 Europe/Paris). The `blockedBy` relationship between the merge-gate lane and review lane AVI-1911 is durably recorded on Paperclip issue AVI-1975 via this change. Gate outcome will be re-evaluated each daily run and posted as a comment or document on AVI-1975.