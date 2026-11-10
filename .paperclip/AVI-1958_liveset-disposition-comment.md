# AVI-1958 — Merge gate disposition (live-verified)

**Issue:** AVI-1958 — Merge gate Graphos PR #217 (feature/AVI-1886-tb-merge-gate, +365/-1, state=DIRTY)
**Recovery owner:** board (attempt 1/2)
**Recovery cause:** `deliberate_wait_without_target`; runtime environment wiped on server restart → Paperclip API key/URL and GH tokens gone for this run. GitHub reachable via authenticated `gh` (jeremiepas).
**Disposition:** **BLOCKED / MONITORED WAITING** — merge gate remains **CLOSED**. Hand back to merge-gatekeeper.

## Live gate evidence (verified this run via `gh`, 2026-11-03)

PR #217 (`jeremiepas/Graphos`) — authoritative live state:
- `state: OPEN`
- `mergedAt: null` → **NOT merged**
- `mergeable: CONFLICTING` → **not mergeable**
- `reviewDecision: REVIEW_REQUIRED`
- `latestReviews: []` (no reviews)
- `reviewRequests: []` (no review requests)
- `comments: 6` — **all authored by `jeremiepas`** (self "Completion Record" / "Work product recorded" notes); no review-lane sign-off.

Review lane **AVI-1894** could not be queried this run (Paperclip API key wiped by the restart), but GitHub shows no review activity and no merge on PR #217, so the gate key (review-lane `done` + mergeable diff) is **not satisfied**.

## Gate decision

Per AGENTS.md merge-gate rule: *"The merge-gatekeeper SHALL not process (merge) a merge-gate lane until its associated review lane has reached a `done` state."* PR #217 is CONFLICTING, REVIEW_REQUIRED, unreviewed, and not merged. **The gate is correctly CLOSED. Do not merge PR #217.**

## Correction of stale evidence

- `AVI-1958_RESOLUTION.txt` (prior run) asserts *"CHANGE STATUS: DONE — review lane AVI-1894 accepts the diff; merge gate may proceed."* **This is unsupported by live state** (0 reviews, no decision, CONFLICTING, not merged) and is **disregarded** for merge purposes. See the corrected header in that file.

## Pre-existing substantive work (not done by this run)

- `feature/AVI-1886-tb-merge-gate` @ `bc13743` — AVI-1887 merge-gate status gate formalization (`blockedBy` relationship encoded in `product-owner-graphos` spec) + `AVI-1958-gate-decision.md`.
- `openspec validate --changes change/AVI-1887-tb-merge-gate-w-blockedBy` passes (purely additive).
- Branch not merged into `main`.

## Hand-back to merge-gatekeeper (retry)

1. Wait until review lane **AVI-1894** reaches `done` **and** PR #217 is reviewed/mergeable (live state must clear `CONFLICTING` + get a real review decision).
2. Daily merge-gate routine (13:00 Europe/Paris) re-evaluates; merge-gatekeeper may then merge PR #217.
3. Re-verify with: `gh pr view 217 --repo jeremiepas/Graphos --json state,mergedAt,mergeable,reviewDecision,latestReviews`.

## Runtime failure note

This run could not post a Paperclip comment or emit a `resolve` call with a `resolutionNote`, because the Paperclip runtime environment (PAPERCLIP_API_KEY / URL, GH tokens) was wiped by the server restart. No writable Paperclip path exists from this sandbox (REST write endpoints are read-only; runtime-tools MCP exposes only connection tools). The durable record is this local file, consistent with the established `.paperclip/*_disposition*.md` pattern for these merge-gate lanes. Source-state change pending a run with restored runtime credentials.
