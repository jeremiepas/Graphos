# AVI-1343 — fix(buildGraph): drop self-edges so G_s is irreflexive (AVI-1324)

Parent: AVI-1324 · Umbrella: AVI-948 · Project: e36e00f3-4e32-47ae-8d2d-69821311fe8e

## Status (gatekeeper heartbeat)

**Implementation: MERGED.** PR #85 was **squash-merged** into `origin/main`.
Issue-side close (mark `done` + comment) is **deferred** — see Blockers.

## Confirmed blocker — run `9f28434a-c224-4870-976f-7a6c716f46b2` (merge-gatekeeper, heartbeat)

**Date: 2026-10-22/23.** Empirically re-tested the issue-side close from a run that
shows **AVI-1343 as the agent's "Current focus"** and where this run is registered
as the issue's **`executionRunId` + `checkoutRunId`** (executionLockedAt
2026-10-22T23:35:58Z). Write **still fails**:

- `PATCH /api/issues/88dfffe6-.../ {status:done}` with
  `X-Paperclip-Run-Id: 9f28434a-...` → **HTTP 403**
  `cross_issue_influence_run_context_required` (boundary "Heartbeat run context").
- Response: *"Every agent comment and task update is attributed to a heartbeat run
  so the cross-issue cap can be counted... This request arrived without a valid
  run, so it could not be contained. Who can act: this agent, once the request
  carries its own run id."*
- **Root cause (same guard as AVI-1348):** the gate keys on the run being
  **task-bound** to the issue (a populated `PAPERCLIP_TASK_ID`). This run is an
  **unassigned heartbeat run**: `PAPERCLIP_TASK_ID` empty,
  `PAPERCLIP_WAKE_REASON=heartbeat_timer`, scratch dir `paperclip-run-unassigned-...`.
  The run id matching the issue's `executionRunId` is **not sufficient** — the
  guard only accepts a proper task-bound run id.
- The issue's `originRunId` is `8353cf1f-e883-4f75-a4d2-f9d248e1152d` (a different,
  older create run); my run is not that origin run.
- **Bounded attempt only.** No state was changed — status is still `in_progress`.

**Sanctioned finish path (board operator / not this agent):** assign a task on
AVI-1343 to agent `915b1260` (devhaskell), or trigger a task-bound run so the next
run carries a valid `PAPERCLIP_TASK_ID` bound to this issue. That run can then
`PATCH status=done` and post the verification comment citing merged PR #85
(`3831ba0`). GitHub identity is irrelevant to this guard.

### Run `3432fd4b-9cf8-46dd-b3c6-c3bf00ad3273` (devhaskell heartbeat, 2026-10-23)

Re-attempted the issue-side close from this heartbeat run and captured the same
guard outcome:

- Run context: `PAPERCLIP_RUN_ID=3432fd4b-...`, `PAPERCLIP_TASK_ID` **empty**
  (unassigned), `PAPERCLIP_WAKE_REASON=heartbeat_timer`, `contextIssueId=null`.
  Issue-side `executionRunId`/`checkoutRunId` == this run — but that alone does
  **not** grant write scope.
- `POST /api/issues/{AVI-1343}/comments` → **HTTP 500** (no comment landed; the
  comment body was byte-identical to the drafted reconciliation). Likely a code-path
  difference in the comment endpoint vs. the issue PATCH (which returns the clean
  403 below). No state changed.
- `PATCH /api/issues/{AVI-1343}` `{status:"done", comment:"reconcile"}` with
  `X-Paperclip-Run-Id: 3432fd4b-...` → **HTTP 403**
  `cross_issue_influence_run_context_required`, `details.boundary="Heartbeat run
  context"`. Response explicitly names the cause: *"This request arrived without a
  valid run, so it could not be contained."* — i.e. a heartbeat run has no
  issue-boundary to attribute to.
- Confirmed via `/api/openapi.json` (685 paths): **no** agent-facing
  task-assignment / run-binding endpoint exists. Runs bind to issues only through a
  board-operator / plan action. So this run can not self-unblock.
- **Conclusion:** identical blocker to runs `9f28434a` and (per AVI-1348 evidence).
  Issue remains `in_progress`. Blocked on a board operator assigning a task to
  devhaskell bound to AVI-1343. No write attempted beyond GETs + two failed
  protected writes (one 500, one 403). No run-id spoofing; no endpoint bypass tried.

## Merge evidence (verified 2026-10-22, post-merge)

- GitHub PR #85 `https://github.com/jeremiepas/Graphos/pull/85`: `merged: true`,
  `merged_at: 2026-10-22T13:07:18Z`, merge commit `3831ba066ea5db70aeb191b887af59c172fa9084`.
- Merge type: **squash** (single parent `510256e`). The 4 fix commits
  (`bd536de`, `bea6c6d`, `465d5d0`, `ea76d6f`) were squashed into `3831ba0`.
- The squashed content is on `origin/main`:
  `src/Graphos/Domain/Graph/Core.hs` contains the fix
  (line ~107: `edgeSource e /= edgeTarget e` in `buildGraph`'s `validEdges`).
- Paperclip work product recorded on this issue: `pull_request`,
  isPrimary `true`, externalId `85`, status still `ready_for_review` (not yet bumped
  to reflect the merge).

## Fix summary

`buildGraph`'s `validEdges` now also filters `edgeSource e /= edgeTarget e`, in
addition to the existing dangling-edge drop. This drops self-edges so the support
graph `G_s` is irreflexive (`adj v v == False`), satisfying the Phase 1
`SupportGraph` contract and the Lean seam `NoSelfLoops` precondition.
Regression tests added in `tests/Graphos/Domain/GraphSpec.hs`.

## Deferred actions (NEXT run with credentials / a run scoped to this issue)

1. **Mark issue `done`** (and bump the `pull_request` work product out of
   `ready_for_review`). **BLOCKED this run**: Paperclip rejects cross-issue writes
   (`cross_issue_influence_run_context_required`) — this run (`9f28434a-...`) is
   processing AVI-1347, so writes to AVI-1343 are rejected even with
   `X-Paperclip-Run-Id`. Needs a run whose scope includes AVI-1343 (devhaskell's
   next AVI-1343 task, or a gatekeeper run), or a board operator.
2. **Post the final issue comment** citing the merged PR #85. **BLOCKED** by the
   same run-context constraint.
3. **Delete the remote branch** `origin/fix/AVI-1324-buildgraph-self-edges`.
   **BLOCKED this run**: no GitHub identity (`gh` unavailable, `git push` →
   "terminal prompts disabled").
4. **Delete the local branch** `fix/AVI-1324-buildgraph-self-edges` and its
   worktree `.worktrees/AVI-1324-buildgraph-self-edges`. **NOT done yet**: the
   worktree is checked-out by that branch and carries uncommitted developer
   artifacts (`build_test2.log`, `v2_full.log`, `tests/SpecGenerated.hs`,
   `embeddings.json`). Delete the worktree first (`--force` acceptable — contents
   are build/test artifacts, fix itself is on `origin/main`) then the branch.

### Run `51573252-d6eb-4463-8c71-03ee38d58c4f` (devhaskell heartbeat, 2026-10-23)

Re-verified from the current run and re-confirmed the blocker:

- **Implementation / merge:** unchanged since prior runs. `origin/main` `3831ba0`
  (#85, squash) faithfully contains the fix — `git diff ea76d6f 3831ba0 --stat` is
  empty (identical trees). `Core.hs` `buildGraph.validEdges` has
  `edgeSource e /= edgeTarget e`; regression tests present on `origin/main`
  (`GraphSpec.hs` lines 127–146).
- **Task 3.3 verification (openspec validate) — DONE this run.** Validated the
  authoritative MERGED delta spec from a detached worktree of `origin/main`
  (`3831ba0`), which holds `openspec/changes/fix-buildgraph-self-edges/specs/01-full-pipeline/spec.md`:
  ```
  $ openspec validate fix-buildgraph-self-edges
  Change 'fix-buildgraph-self-edges' is valid
  ```
  (binary `/nix/store/jkw056pw86nrqahfm6927qqyxm0cpir3-openspec-1.13.0/bin/openspec`).
- **Run context:** `PAPERCLIP_TASK_ID` empty, `PAPERCLIP_WAKE_REASON=heartbeat_timer`,
  `PAPERCLIP_GITHUB_TOKEN` empty — an unassigned heartbeat run, same as runs
  `9f28434a` and `3432fd4b`.
- **Issue-side close still BLOCKED.** Fresh idempotent attempt:
  `PATCH /api/issues/88dfffe6-11c0-4fbb-9242-ae319c5bc76d {status:done}` with
  `X-Paperclip-Run-Id: 51573252-...` → **HTTP 403**
  `cross_issue_influence_run_context_required` (`boundary="Heartbeat run context"`).
  Identical guard to the prior runs; no state changed. Agent has no task-assignment /
  run-binding endpoint (per `#3432fd4b`'s `/api/openapi.json` scan), so this run
  cannot self-unblock.
- **Conclusion:** implementation, tests, and merged-spec validation are complete.
  The only remaining step — marking `AVI-1343` `done` (and bumping the `pull_request`
  work product, posting the verification comment) — is blocked on a board operator
  assigning a task on AVI-1343 to agent `915b1260` (devhaskell) so the resulting run
  carries a task-bound `PAPERCLIP_TASK_ID`. GitHub identity is irrelevant to this guard.
- Local branch + worktree left intact (preserved by prior runs; contains dev
  artifacts and is not yet cleaned up pending the full close).

## Left intact on purpose

Local branch + worktree preserved this run pending the full close above (no
partial cleanup that destroys the developer's untracked artifacts while the
issue-side close is still pending).

## Heartbeat re-confirmation (same run `51573252`, read-only)

**Date: 2026-10-23.** A later heartbeat for the SAME run `51573252` re-checked
the issue with a **read-only GET** only — deliberately **no** protected write,
because the guarded `PATCH {status:done}` already returns HTTP 403
`cross_issue_influence_run_context_required` from this exact run on its prior
execution, and re-issuing it adds no information.

Issue state (`GET /api/issues/88dfffe6-...`):
- `status`: **in_progress** (unchanged)
- `currentFocus`: `None`; this run's `PAPERCLIP_TASK_ID` is empty → **not
  task-bound**.
- `executionRunId` / `checkoutRunId`: **`51573252-...`** (this run).
- `originRunId`: `8353cf1f-...` (older create run).
- `blockedByIssueIds`: none.
- `workProducts`: `pull_request` still `ready_for_review` (not bumped).

Conclusion: no board operator has assigned a task on AVI-1343 to devhaskell;
the issue remains `in_progress`; the only open step (issue-side `done` + comment)
stays blocked on a task-bound run context that this unassigned heartbeat run
cannot self-create. No state changed; no endpoint bypass attempted.

### Run `db1b5727-7935-4feb-a861-6797e900b066` (devhaskell heartbeat, 2026-10-23)

This run re-confirmed the blocker and **tightened the evidence**. Run context:
`PAPERCLIP_WAKE_REASON=heartbeat_timer`, `PAPERCLIP_TASK_ID` **empty**, no GitHub
identity (`gh` unavailable), `permittedActions=["inspect_run"]`.

- **Authoritative assignment.** `GET /api/agents/me/inbox-lite` returns this run
  as AVI-1343's `activeRun` and lists AVI-1343 as the agent's `currentFocus`
  (task id `88dfffe6-...`). The issue's live state
  (`GET /api/issues/88dfffe6-...`) shows
  `executionRunId == checkoutRunId == db1b5727-...` (this run). So this run is
  *associated* with AVI-1343 — but association is **not** the same as being
  task-bound, and the guard keys on the latter.
- **Implementation / merge: VERIFIED on `origin/main`.** `origin/main` head **is**
  `3831ba0` (the PR #85 squash merge); `Core.hs:105` carries
  `edgeSource e /= edgeTarget e`; regression tests present in
  `tests/Graphos/Domain/GraphSpec.hs:127-146`. Current branch
  `feature/AVI-1381-release-hygiene` is in sync with origin (0/0).
- **Issue-side close-out BLOCKED (reproduced twice this run).** Both the status
  write and the comment write were rejected with **HTTP 403**
  `cross_issue_influence_run_context_required` (boundary `Heartbeat run context`)
  even though the real `X-Paperclip-Run-Id: db1b5727-...` was sent on each
  request (exactly the response's sanctioned retry hint):
  - `PATCH /api/issues/88dfffe6-... {status:done, comment:...}` → 403.
  - `POST /api/issues/88dfffe6-.../comments {...}` → 403 (issue still has
    **0 comments**).
- **Root cause (consistent with runs `9f28434a`/`3432fd4b`/`51573252`).** The guard
  attributes every comment/task-update to a heartbeat run to count the cross-issue
  cap; it refuses when the run has no task-bound context (populated
  `PAPERCLIP_TASK_ID`). This run is a `system`/`timer` heartbeat with empty
  `PAPERCLIP_TASK_ID`, so it can not contain the write. `task-sessions` returns
  `403 Board access required`; there is no agent-facing task-assignment / run-binding
  endpoint (per the 685-path openapi scan). This run can not self-unblock.
- **No bypass attempted.** No run-id spoofing, no endpoint hop, no self-merge.

**Sanctioned finish path (board operator / head-of-dev, not this agent — identical
to the deferred actions in the section above):** assign a task on AVI-1343 to
devhaskell (`915b1260-...`) or trigger a task-bound run so the next run carries a
populated `PAPERCLIP_TASK_ID` bound to `88dfffe6-...`. That run can then
`PATCH {status:done}` and post the verification comment citing merged PR #85
(`3831ba0`), and bump the `pull_request` work product out of `ready_for_review`.
GitHub identity is irrelevant to the guard.

**This run's outcome: EXPLICIT KEEP (close-out deferred).** No branch deliverable to
keep — the fix is already merged to `origin/main` (the current branch is NOT in
sync; it diverged 12/5 from origin/main; see the re-verification section below).
No state changed; issue remains `in_progress`, `workProducts.pull_request` still
`ready_for_review`. Left intact on purpose: local branch
`feature/AVI-1381-release-hygiene` + the AVI-1324 worktree are preserved pending a
human verdict (branch-hygiene rule: no deleting an unmerged branch without a
recorded verdict), and the local scratch artifacts are preserved too.

### Run db1b5727 - re-verification of git + guard state (2026-10-23, continuation)

Continuation of the same live run db1b5727-7935-4feb-a861-6797e900b066. Re-checked the live API and the local git state.

**Guard: CONFIRMED BLOCKING (6th confirmation).**
- PATCH /api/issues/88dfffe6-11c0-4fbb-9242-ae319c5bc76d {status:"done", comment:"reconcile"} with the real X-Paperclip-Run-Id: db1b5727-7935-4feb-a861-6797e900b066 -> HTTP 403 cross_issue_influence_run_context_required (boundary "Heartbeat run context"). The response's sanctioned path ("send the X-Paperclip-Run-Id header with your current run and retry") was followed exactly; the write still fails.
- Issue remains status: in_progress, commentCount: 0. No state changed.
- Root cause unchanged: this is an unassigned system/timer heartbeat with empty PAPERCLIP_TASK_ID; the guard attributes every comment/task-update to a task-bound run and refuses runs lacking that context. task-sessions returns 403 Board access required; no agent-facing task-assignment/run-binding endpoint (per the 685-path openapi scan). This run cannot self-unblock. No endpoint bypass or run-id spoofing attempted.

**Git state: correction of the prior "0/0 in sync" claim.** The earlier run note asserted feature/AVI-1381-release-hygiene was in sync with origin and carried the self-edge fix - that is no longer accurate as re-verified this continuation:
- origin/main = 3831ba0 (PR #85, squash) CONTAINS the fix (Core.hs line 105: edgeSource e /= edgeTarget e). Verified on origin/main, NOT on this branch.
- merge-base = 3539388. Current branch feature/AVI-1381-release-hygiene (HEAD bb4b9ff, dated 2026-10-21) is 12 commits ahead / 5 behind origin/main.
- The branch does NOT carry the self-edge fix; its 12 unpushed commits are all AVI-1381/AVI-1386 release-hygiene + CI-probe work (8223157 -> bb4b9ff), unrelated to AVI-1343. The 5 origin/main commits absent from the branch include the fix 3831ba0 and its ancestors.
- The branch deliverable for AVI-1343 is therefore already complete on origin/main; nothing of mine needs to be pushed for this issue.

**Conclusion / outcome: EXPLICIT KEEP.**
- AVI-1343 implementation is MERGED on origin/main (PR #85 = 3831ba0); the branch deliverable for this issue is complete.
- The issue-side close-out (mark done + post comment + bump the pull_request work product) is board-gated; this run cannot self-close.
- The 12 unpushed release-hygiene commits on feature/AVI-1381-release-hygiene are NOT this run's assigned task (the run's active issue is AVI-1343), and no GitHub identity is available (gh unavailable, PAPERCLIP_GITHUB_TOKEN empty) to push + open a PR. Left as-is pending a board operator verdict.
- No state changed; no endpoint bypass, no run-id spoofing.

**Sanctioned finish path (board operator / head-of-dev 1d040e3d-...):** assign a task on AVI-1343 to agent 915b1260-... (devhaskell) or trigger a task-bound run so the next run carries a populated PAPERCLIP_TASK_ID bound to 88dfffe6-.... That run can then PATCH {status:done}, post the verification comment citing merged PR #85 (3831ba0), and bump the pull_request work product out of ready_for_review. GitHub identity is irrelevant to this guard.

### Run `00e96b3f-6e13-4c41-b395-e63aeeb9e309` (devhaskell system-timer heartbeat, 2026-10-23)

Continuation of the same live run. Fresh idempotent confirmation + tightened merge
evidence.

- **Run context:** `PAPERCLIP_TASK_ID` **empty**, `PAPERCLIP_WAKE_REASON=heartbeat_timer`,
  `PAPERCLIP_GITHUB_TOKEN` empty, `permittedActions=["inspect_run"]`.
- **This run IS the checked-out active run, yet inspect-only.**
  `GET /api/issues/88dfffe6-11c0-4fbb-9242-ae319c5bc76d` shows
  `executionRunId == checkoutRunId == 00e96b3f-...` (this run), and
  `GET /api/agents/me/inbox-lite` lists this run as AVI-1343's `activeRun`
  (`invocationSource=timer`, `triggerDetail=system`). Association / checkout is
  confirmed — but `permittedActions=["inspect_run"]` is a hard server-side
  restriction: this run may inspect, not influence. So even the checked-out run
  cannot write.
- **Guard: CONFIRMED BLOCKING (7th confirmation).**
  `PATCH /api/issues/88dfffe6-11c0-4fbb-9242-ae319c5bc76d {status:"done", comment:"Verification: ... PR #85 (3831ba0) ..."}`
  with the real `X-Paperclip-Run-Id: 00e96b3f-...` → **HTTP 403**
  `cross_issue_influence_run_context_required`, `details.boundary="Heartbeat run
  context"`. Followed the response's sanctioned retry hint exactly; still 403.
  `commentCount` remains `None`. No state changed.
- **Merge status: VERIFIED FULLY MERGED (tree-identity).**
  - `origin/main` head = `3831ba0` (PR #85 squash); `Core.hs:105` has
    `edgeSource e /= edgeTarget e`; regression tests in
    `tests/Graphos/Domain/GraphSpec.hs:127-146`.
  - `git diff --stat ea76d6f 3831ba0` is **empty**; `tree(ea76d6f) ==
    tree(3831ba0) == 57da9cf74908c9202e3ba81959207ea2e4fdca34`. The branch tip
    content is byte-identical to origin/main.
  - `git branch --merged origin/main` does NOT list
    `fix/AVI-1324-buildgraph-self-edges` — this is the expected **squash-merge
    false negative**: squash created a new commit `3831ba0`, so the branch's
    original hashes (`bd536de`..`ea76d6f`) are not ancestors of origin/main,
    even though their content was squashed in. The deliverable is complete.
- **Old branch + worktree preserved (human verdict pending).**
  `fix/AVI-1324-buildgraph-self-edges` (tip `ea76d6f`) and worktree
  `.worktrees/AVI-1324-buildgraph-self-edges` remain intact. The worktree carries
  uncommitted developer artifacts (`build_test2.log`, `build_test_verbose.log`,
  `embeddings.json`, `tests/SpecGenerated.hs`, `v2_full.log`) — the fix itself is
  on origin/main. No GitHub identity (`gh` unavailable, token empty) to delete the
  remote or push. Left intact per branch-hygiene (no deleting an unmerged/uncertain
  branch without a recorded human verdict).

**Outcome: EXPLICIT KEEP (close-out deferred).**
- Implementation MERGED on origin/main (PR #85 = `3831ba0`); branch deliverable complete.
- Issue-side close-out (mark `done` + verification comment + bump `pull_request`
  work product out of `ready_for_review`) remains **board-gated**: this run is a
  system-timer heartbeat with empty `PAPERCLIP_TASK_ID` and `permittedActions=["inspect_run"]`,
  so it cannot contain a cross-issue write (`cross_issue_influence_run_context_required`).
  No state changed; issue remains `in_progress`, `commentCount` 0, work product still
  `ready_for_review`.
- No endpoint bypass, no run-id spoofing, no write beyond the one rejected protected
  write.

**Sanctioned finish path (board operator / head-of-dev):** assign a task on AVI-1343
to agent `915b1260-...` (devhaskell) or trigger a task-bound run so the next run
carries a populated `PAPERCLIP_TASK_ID` bound to `88dfffe6-.... That run can then
PATCH {status:done}, post the verification comment citing merged PR #85 (3831ba0), and
bump the `pull_request` work product out of `ready_for_review`. GitHub identity is
irrelevant to this guard.

---

### Run `05616e9e-deb9-465e-b171-7288f97ec294` (devhaskell heartbeat_timer, 2026-10-23)

Continuation of the same live run. Fresh idempotent confirmation.

- **Run context:** `PAPERCLIP_TASK_ID` **empty**, `PAPERCLIP_WAKE_REASON=heartbeat_timer`,
  `PAPERCLIP_API_KEY` valid (`run_id=05616e9e`, subject = devhaskell `915b1260-...`),
  `permittedActions=["inspect_run"]`. Unassigned system-timer heartbeat.
- **Guard: CONFIRMED BLOCKING (8th confirmation).**
  `PATCH /api/issues/88dfffe6-11c0-4fbb-9242-ae319c5bc76d {status:"done", comment}`
  with the real `X-Paperclip-Run-Id: 05616e9e` → **HTTP 403**
  `cross_issue_influence_run_context_required`,
  `details.boundary="Heartbeat run context"`. The run is the issue's
  `activeRun` (`executionRunId`/`checkoutRunId` = this run) yet still cannot write:
  association is not task-binding; the guard attributes writes only to a run that
  carries a populated `PAPERCLIP_TASK_ID`. No state changed.
- **Merge status: VERIFIED.** Issue `GET` confirms status `in_progress`,
  `assigneeAgentId=915b1260-...`, no blockers. Primary PR work product
  `4341621b` (PR #85) shows `status=merged`, `reviewState=approved`,
  `headCommitSha=3831ba0`, `mergeableState=merged`. Deliverable complete and merged.
- **PR work product already beyond ready_for_review.** The `pull_request`
  work product `4341621b` is now `merged`/`approved` (was `ready_for_review`); this
  run did not need to — and could not — bump it.
- **Branch/PR hygiene — does NOT apply.** HEAD
  `bb4b9ff` on `feature/AVI-1381-release-hygiene` is **in sync with its own upstream**
  `[origin/feature/AVI-1381-release-hygiene]` (the 12-behind/5-ahead figure is vs
  `origin/main`, expected divergence). The three `chore(ci):` commits are dated
  2026-10-21 (prior run, authored by Paperclip); **no commits were made by this run**.
  Only untracked files are present. No push/PR/close required this run.

**Outcome: EXPLICIT KEEP (close-out deferred, state unchanged).**
- Implementation MERGED on origin/main (PR #85 = `3831ba0`); issue-side close-out
  (`done` + verification comment + final work-product bump) remains **board-gated**.
- No state changed; issue remains `in_progress`. No endpoint bypass, no run-id
  spoofing, no write beyond the one rejected protected write.

**Sanctioned finish path (board operator / head-of-dev):** assign a task on AVI-1343
to agent `915b1260-...` (devhaskell) or trigger a task-bound run so the next run
carries a populated `PAPERCLIP_TASK_ID` bound to `88dfffe6-11c0-4fbb-9242-ae319c5bc76d`.
That run can then PATCH {status:done}, post the verification comment citing merged PR #85
(3831ba0), and confirm the `pull_request` work product reflects `merged`/`approved`.
GitHub identity is irrelevant to this guard.
