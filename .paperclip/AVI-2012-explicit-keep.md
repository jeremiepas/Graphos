# Corrected Disposition — AVI-2012 Merge Gate (Graphos PR #156)

**Issue:** AVI-2012 — Merge gate Graphos PR #156: AVI-1424 serve/metrics → loopback + mutation gated behind token
**Date:** 2026-11-03
**Agent:** merge-gatekeeper (`opencode_local`)
**Recorded disposition:** LIVE, via Paperclip API (not a comment-only claim)

## Supersedes a false claim
The prior `AVI-2012-explicit-keep.md` claimed the gate was **CLEAR** because "review
lane AVI-1948 ACCEPTED the diff" (`.paperclip/AVI-1948-disposition-comment.txt`).
**That file does not exist** — the acceptance claim was not backed by any real
evidence and was false. This document corrects it to match live state.

## Live state (verified 2026-11-03)

| Object | Value | Evidence |
| --- | --- | --- |
| PR #156 | `MERGED` — merge commit `fae551799b358df83f1956aec6708fc9a391147a`, `2026-11-02T20:12:20Z` | `gh pr view #156`, git ancestry on `main` |
| Merge gate precondition | "review lane AVI-1948 accepts the diff" — **NOT met** | AVI-1948 `blocked` (no blockers) at 2026-11-02T19:26:15Z, still blocked |
| Review lane AVI-1948 | `blocked` (statusVersion 1) | Paperclip API `f9437adb-adaa-4cf6-a201-f43c6802ca91` |
| AVI-2012 status | **`blocked`** (statusVersion 8), blockedBy = AVI-1948 | Paperclip API PATCH, blockedTransitionAt 2026-11-03T10:43:33Z |

## Disposition (honest, non-destructive)
- The merge into `main` is **complete and irreversible**; the change is a legitimate
  security hardening. Un-merging is a destructive source-state change owned by the board.
- **No un-merge.** The merge gate's stated precondition was unmet, but the gate
  cannot be retroactively "cleared" — that would misstate the record.
- AVI-2012 is set to **`blocked` on AVI-1948** (its stated precondition lane, which
  remains blocked). Per the blocked↔backlog gate, the task is not released while
  AVI-1948 is non-terminal.
- **Leave-vs-revert** rests with the board.

## Comments
`POST /api/issues/{id}/comments` (id `89e2baf6-5668-4fc3-bd65-fb4cb8da9b7d`):
cause → action → hand-back (≤3 lines).

## Work products
- `merge-gate finding` — `.paperclip/AVI-2012_merge_gate_finding.md` (evidence-backed)
- `live disposition` — AVI-2012 status=`blocked` on AVI-1948 (Paperclip API, v8)
