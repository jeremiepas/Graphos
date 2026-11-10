# PR #200 — feature/AVI-1731-definition-of-done (Graphos)

**Issue:** AVI-1975 — Merge gate Graphos PR #200: feature/AVI 1731 definition of done

**Branch:** `feature/AVI-1975-merge-gate-pr-200-blockedBy-avi1910`

**Status:** BLOCKED — awaiting review lane AVI-1911 (`done`).

**Gate logic (merge-gatekeeper SHALL NOT merge PR #200 until AVI-1911 = done):**
- Review lane AVI-1911 must reach `done`.
- If not, the merge-gate lane stays in its current state and is not merged.
- The `blockedBy` relationship between the merge-gate lane and review lane AVI-1911 is recorded durably on Paperclip issue AVI-1975 (see `.paperclip/AVI-1975-disposition-comment.txt`, `.paperclip/AVI-1975_resolution-note.md`).
- Re-evaluated each daily run (13:00 Europe/Paris).

**Why blocked:** The review lane AVI-1911 has not accepted the diff yet; self-approval was blocked by GitHub policy (PR authored by jeremiepas). No merge performed.

**Gate outcome to record:** When AVI-1911 = done, gate opens → eligible for merge. Otherwise stays blocked.

**Verification:**
- `openspec check` passes for the new change.
- PR documentation registered at origin/main via `scripts/post-github-review.sh 200 COMMENT ...`.
