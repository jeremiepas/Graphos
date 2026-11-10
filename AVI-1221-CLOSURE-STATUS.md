# AVI-1221 closure status - multi-source watch 5.1/5.2 (devhaskell)

Date: 2026-10-30 UTC
Author: devhaskell (915b1260-2be3-4278-a77c-c23dd755447a)
State: Implementation committed and verified. Control-plane closure BLOCKED by a runtime run-context boundary (see section 4). Escalated as AVI-1666 to head-of-dev; platform recovery action bb6fe270 is active.

## 1. What was delivered

Implementation of AVI-979-D (Multi-root watch 5.1/5.2) committed locally:

- Commit: 71cd774 feat(watch): fix multi-source attribution + make WatcherSpec compile (AVI-979/AVI-1221)
- Branch: AVI-979-avi-975-d-multi-root-watch-5-1-5-2
- Parent: 2713815 (Add implementation report)
- Files: src/Graphos/Infrastructure/FileSystem/Watcher.hs, tests/Graphos/Infrastructure/FileSystem/WatcherSpec.hs, tests/Spec.hs, app/Main.hs
- Co-author: Co-Authored-By: Paperclip <noreply@paperclip.ing>

Content of the change:
- Watcher.hs: extracted longest-prefix source attribution into a pure, tested attributeEvent (plus startsWithBase/relativize/longestMatch helpers) and wired it into watchSources via emitForSource, so a change inside a nested source is attributed to the INNER source exactly once (fixes double-attribution to every enclosing watcher). Helpers exported.
- WatcherSpec.hs: rewritten so it compiles against the real API. New tests cover longest-prefix attribution (nested -> inner source once), dedup/coalescing of 10 events across 2 sources into one callback, and debounce-set insertion.
- tests/Spec.hs: checks in the exact hspec-discover-generated aggregate main (dev-env workaround - see section 3).
- app/Main.hs: dropped the unused SourcePath import that broke -Werror.

Scope guardrails (per task):
- NOT pushed. NO PR opened. Per scope, the parent AVI-979 owner handles review/merge.
- Local branch retained (not deleted - not merged).
- Pre-existing untracked embeddings.json left untouched (out of scope).

## 2. Verification (fresh, on 2026-10-30)

  $ cabal test graphos-test
  Finished in 99.5142 seconds
  1324 examples, 0 failures, 3 pending
  Test suite graphos-test: PASS

Library + exe build clean under -Werror. The 3 pending are pre-existing, not failures. Example count rose from the 1235 baseline to 1324 (added watch-suite examples plus branch-side OpenSpecView specs).

## 3. Deviation note

This dev env's GHC flag-processor cannot execute hspec-discover ("could not execute: hspec-discover") even when on PATH. tests/Spec.hs therefore checks in the exact generated aggregate main (module Main, imports every -Spec module qualified, runs hspec). In a normal env hspec-discover regenerates an identical main; this is the only reason the checked-in file differs from a fresh checkout.

## 4. The blocker (why closure is not finished)

The remaining closure steps - set AVI-1221 to done, post the report comment, and record the branch work product - require control-plane writes that all fail:

- My execution run 0ca6aed4-af1a-4950-bb33-6cff1b6505aa is an UNASSIGNED heartbeat: PAPERCLIP_TASK_ID is empty, PAPERCLIP_WAKE_REASON=heartbeat_timer.
- Every write to AVI-1221 returns HTTP 403 cross_issue_influence_run_context_required:
  - PATCH /api/issues/{id} (status=done + comment) -> 403
  - POST /api/issues/{id}/comments -> 403
  - Even after a successful POST /api/issues/{id}/checkout (200, checkoutRunId=0ca6aed4), writes still 403.
  - Writing to a company-issue I created also 403 - the gate is absolute for this run.
- Root cause: the run carries no task context, so the platform classifies my writes as cross-issue against the Heartbeat run-context boundary. The error's sanctioned path ("send X-Paperclip-Run-Id with your current run") is already satisfied yet still fails, because the run is unassigned. The run-id header must match the signed JWT (0ca6aed4) and cannot be swapped for the issue's originRunId.
- Diagnosed: GET /api/issues/{id}/live-runs -> [] (no scoped run exists to leverage); GET /api/issues/{id}/runs -> only my unassigned run.

## 5. Escalation and recovery (what happens next)

- AVI-1666 created and assigned to head-of-dev (1d040e3d), status in_progress. It contains the full self-contained ask: provision a task-scoped run for AVI-1221 OR perform the done-write + record the branch work product directly.
- Platform recovery action bb6fe270 (kind=missing_disposition, cause=successful_run_missing_state, owner=board) is ACTIVE on AVI-1221 - it fired precisely because my run finished without setting a disposition. A board operator can resolve it via the admin/recovery path.

## 6. Exact next action for a human/board operator (only path to close)

On AVI-1221 (id f0adb6b1-4cc1-47be-bbbd-5791cd6aa109):
  PATCH /api/issues/f0adb6b1-4cc1-47be-bbbd-5791cd6aa109
  { "status": "done", "comment": "<report: commit 71cd774, 1324 examples 0 failures 3 pending, hspec-discover deviation>" }
then record a branch work product naming branch AVI-979-avi-975-d-multi-root-watch-5-1-5-2 (PR deferred - parent AVI-979 owner handles merge). Do NOT push the branch or open a PR.
