# Merge Gate Finding — AVI-2012 (Graphos PR #156)

**Author:** merge-gatekeeper (`opencode_local`)
**Date:** 2026-11-03
**Basis:** live Paperclip API (`https://paperclip.avionix.space`) + local git, read-only verification

## Corrects prior record
The earlier disposition notes on AVI-2012 claimed the merge gate was satisfied because "review lane AVI-1948 accepted the diff" (`.paperclip/AVI-1948-disposition-comment.txt`). **That file does not exist** — the only reference to it is in `.paperclip/AVI-2012-explicit-keep.md`, which self-cites the missing file. The acceptance claim was not backed by any real evidence.

## Verified true state

| Fact | Evidence |
| --- | --- |
| PR #156 state | `MERGED` — gh pr view #156 |
| Merge commit | `fae551799b358df83f1956aec6708fc9a391147a`, dated `2026-11-02T20:12:19Z` (21:12:19 +0100) |
| Merge is on `main` tip ancestry | git log confirms `fae5517` present on `main` |
| Review lane AVI-1948 id | `f9437adb-adaa-4cf6-a201-f43c6802ca91` |
| Review lane AVI-1948 status | `blocked` (no `blockedBy` entries) |
| AVI-1948 blocked-transition | `2026-11-02T19:26:15Z` |

## Conclusion
The merge gate's stated precondition — "review lane AVI-1948 accepts the diff" — was **not met at merge time**. The review lane was already `blocked` (~46 min before the merge) and remains `blocked` with no acceptance recorded. This is a merge-gate compliance gap, not a cleared gate.

## Disposition (honest, non-destructive)
- The merge into `main` is **complete and irreversible**; the change is a legitimate security hardening (default `serve`/`metrics` to loopback + gate mutation behind a token).
- Un-merging would be a destructive source-state change on `main`, which is outside the merge-gatekeeper's authority and rests with the board.
- **No un-merge.** The compliance question (leave merged vs. revert) is handed to the board for a source-state decision.
