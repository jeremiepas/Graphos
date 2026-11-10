## Context

The Graphos release policy (§1, §2) specifies that every PR must carry a live review lane. The daily Graphos PR intake watchdog (AVI-1876) ensures that every open PR carries a live review lane and creates missing lanes same day. However, the relationship between the review lane and the PROBE check is not codified — the PROBE verification (e.g., intentgraph non-triviality probe or similar validation) may be run as part of the daily routine, but there is no explicit gate requiring it to be `done` before the review lane can be marked `done`. This creates ambiguity: reviewers may mark a review lane `done` before PROBE completes, or may not know whether PROBE is part of the review acceptance criteria.

Since the parent issue AVI-1876 (Daily Graphos PR intake watchdog) ensures that every open PR carries a live review lane and creates missing lanes same day, adding a PROBE gate to the review lane is the natural formalization: the review lane should not transition to `done` until its associated PROBE check has also reached `done`.

## Goals / Non-Goals

**Goals:**
- Gate each review lane's processing on PROBE-check resolution: `done` → review lane may proceed to `done`; any other state → review lane stays in current state (do not mark `done`).
- Require the `blockedBy` relationship between review lane and PROBE check be recorded durably on each Paperclip issue, so the gate decision is not held only in prose.
- Keep the change purely additive to the existing `product-owner-graphos` spec and release policy.
- No Haskell build/test is required; the change is spec-only.

**Non-Goals:**
- Changing Paperclip's own status-machine semantics; this documents review lane behavior, it does not modify how the board stores statuses.
- Altering existing requirements in the `product-owner-graphos` spec or any other live spec.
- Writing or reviewing Haskell code, or changing the build/test pipeline.
- Deciding which statuses exist on the board (that is a company/board decision).

## Decisions

- **Encode the gate as OpenSpec `SHALL` requirements + WHEN/THEN scenarios** in the `product-owner-graphos` spec (the canonical home for PO/merge-gate behavior), rather than scattering prose across chat comments.
  - *Alternative considered:* a free-text rule in the `AGENTS.md` bundle — rejected because it is not testable, not versioned with the change, and not reviewable via `openspec check`.
  - *Alternative considered:* a Paperclip board automation rule — rejected because the board's status machine is out of the review lane's authority; the PO only triages.
- **All-or-nothing gating** (a PROBE check that is not `done` does NOT allow the review lane to be marked `done`).
  - *Alternative considered:* unblock when PROBE is `in_progress` — rejected, it would allow marking review lanes as complete before PROBE is finished.
- **Record every gate outcome durably** (comment or document) so partial or no-change decisions survive across heartbeats.
  - *Alternative considered:* only act on changes — rejected, silent no-ops leave the rationale unreconstructable.

## Verification

- `openspec check` (or equivalent validation) passes for the new change: every requirement uses `SHALL`/`MUST` and every scenario uses exactly four `####` headers with `WHEN`/`THEN`.
- Manual read-through confirms the gate logic matches the AVI-1892/AVI-1891 objective and is consistent with the existing `product-owner-graphos` requirements (no contradictions, no modified requirements).
- No Haskell build/test is required; the change is spec-only.