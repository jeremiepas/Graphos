## Context

The Graphos release policy (§1, §2) specifies that every PR must carry a merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane. The daily Graphos merge gate routine (13:00 Europe/Paris) processes PRs that are reviewed and mergeable. However, the `blockedBy` relationship between the merge-gate lane `pr89-check` (for Graphos PR #210) and the review lane AVI-1901 is not codified as an explicit gate — it is only mentioned in prose in the release policy. This creates ambiguity about when the merge-gatekeeper may proceed to merge PR #210 through the merge gate.

Since the parent issue AVI-1876 (Daily Graphos PR intake watchdog) ensures that every open PR carries a live review lane and creates missing lanes same day, the merge gate `blockedBy` relationship is the natural next formalization: gating the merge-gate lane `pr89-check`'s processing on the review lane AVI-1901 being `done`.

## Goals / Non-Goals

**Goals:**
- Gate the merge-gate lane `pr89-check`'s processing on review-lane AVI-1901 resolution: `done` → proceed to merge; any other state → stay in current lane state (do not merge).
- Require the `blockedBy` relationship between merge-gate lane `pr89-check` and review lane AVI-1901 be recorded durably on each Paperclip issue, so the gate decision is not held only in prose.
- Keep the change purely additive to the existing `product-owner-graphos` spec and release policy.
- No Haskell build/test is required; the change is spec-only.

**Non-Goals:**
- Changing Paperclip's own status-machine semantics; this documents merge-gatekeeper behavior, it does not modify how the board stores statuses.
- Altering existing requirements in the `product-owner-graphos` spec or any other live spec.
- Writing or reviewing Haskell code, or changing the build/test pipeline.
- Deciding which statuses exist on the board (that is a company/board decision).

## Decisions

- **Encode the gate as OpenSpec `SHALL` requirements + WHEN/THEN scenarios** in the `product-owner-graphos` spec (the canonical home for PO/merge-gate behavior), rather than scattering prose across chat comments.
  - *Alternative considered:* a free-text rule in the `AGENTS.md` bundle — rejected because it is not testable, not versioned with the change, and not reviewable via `openspec check`.
  - *Alternative considered:* a Paperclip board automation rule — rejected because the board's status machine is out of the merge-gatekeeper's authority; the PO/merge-gatekeeper only triages.
- **All-or-nothing gating** (a review lane AVI-1901 that is not `done` does NOT allow the merge-gate lane `pr89-check` to proceed).
  - *Alternative considered:* unblock when the review lane AVI-1901 is `in_progress` — rejected, it would allow merging PRs whose review is incomplete.
- **Record every gate outcome durably** (comment or document) so partial or no-change decisions survive across heartbeats.
  - *Alternative considered:* only act on changes — rejected, silent no-ops leave the rationale unreconstructable.

## Verification

- `openspec check` (or equivalent validation) passes for the new change: every requirement uses `SHALL`/`MUST` and every scenario uses exactly four `####` headers with `WHEN`/`THEN`.
- Manual read-through confirms the gate logic matches the AVI-1965 objective and is consistent with the existing `product-owner-graphos` requirements (no contradictions, no modified requirements).
- No Haskell build/test is required; the change is spec-only.