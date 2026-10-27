# AVI-1348 — Route 1 (G5 SCC/condensation GAP-1) working notes

## Status this run (heartbeat `eb80a767`, legacy/timer, assigned to me)

Route 1 is **fully implemented and verified** on branch
`AVI-512-add-math-in-graphos-project` @ `63a2824`.

### Verified (fresh this run)
- `cabal build graphos-test` → BUILD_EXIT=0 (GHC 9.10.3, incremental).
- `cabal test graphos-test` → **PASS: 1331 examples, 0 failures, 3 pending**
  (the 3 pending are real-corpus oracle skips, unrelated to this issue).
- All acceptance tests pass: SCC §8 golden `{a,b,c}/{d,e}/{f}`; condensation
  chain `{a,b,c}→{d,e}→{f}` (Thm 2.3); isAcyclicDirected (INV-C5, self-loop F3,
  property); hasCycleUndirected F1/F2/F3/AF-3/F7 + INV-C2 soundness;
  isolatedNodes (INV-C3); connectedComponents/weaklyConnectedComponents
  (INV-C1, INV-C6); INV-7/8/9/10.

### Surface (Graphos.Domain.Graph.Query, exported from Graphos.Domain.Graph)
- `cond`/`condWithCached`, `isAcyclicDirected`/`*WithCached`,
  `connectedComponents`/`*WithCached`, `weaklyConnectedComponents`/`*WithCached`,
  `isolatedNodes`/`*WithCached`, `hasCycleUndirected`/`*WithCached`.
- Retained (requirement-creation pins, no reimplementation):
  `stronglyConnectedComponents(+WithCached)`,
  `shortestPathReachable(+WithCached)`.

### Specs
- `openspec/specs/**` requirements untouched. Diff vs branch base `d927d8d` = 4
  files: `Query.hs` (G5 surface), `Graph.hs` (exports), `SCCSpec.hs` (new tests
  module, registered in graphos.cabal), and a +24-line doc adopt of the
  already-implemented `hasCycleUndirected` for register consistency. No
  requirement edits.

## Mandatory finish step — satisfied (explicit keep)
Existing `branch` work product `662e7ffb` on this issue is **active / healthy**:
it names the branch, records implementation+verification, and explicitly
defers the PR ("no run-scoped GitHub identity this run (AVI-929), so no PR
opened"). No duplicate work product needed.

## BLOCKED — could not flip status to `done` or post the verification comment
Both `PATCH /api/issues/{id}` (status=done) and
`POST /api/issues/{id}/comments` return **403
`cross_issue_influence_run_context_required`** (boundary "Heartbeat run
context"), even though `X-Paperclip-Run-Id: eb80a767` is sent and matches
`executionRunId`/`checkoutRunId`.

Root cause: the guard keys on the issue's **origin run**,
`originRunId = c3b2a120-8f62-455e-8243-f67c4419c0b6`, which is a different
(hfinished) run. My run is only the execution/checkout run and is a
legacy/timer run lacking the origin-run attribution context the guard needs.
No in-run workaround exists; stopped after one bounded attempt per each.

## Run `9d5c0a1e` (heartbeat, `PAPERCLIP_TASK_ID` empty) — state reconfirmed
**Nothing changed; blocker persists. No new commits made.**

- Issue `652c436b` title is now **"AVI-1323 Route 1: implement G5 SCC/condensation
  GAP-1 surface (specs unchanged)"** — the board consolidated Route 1 under the
  AVI-1323 umbrella. Issue ID unchanged; status still `in_progress`.
- A new child task `009879b8` **"AVI-1323 Route 2: G6 PROP-EQV incremental
  reconciliation + deleteNodeIncident"** was created (status `blocked`). Board
  has moved on to Route 2; Route 1 (this issue) is done but un-flipped.
- WPs both still **active**: `pull_request` `204fb33f` (PR #86), `branch`
  `662e7ffb`.
- **Branch fully intact + pushed**: worktree tip `63a28243f9c66c86e28f3a3c7b760e289a5fd738`
  == `origin/AVI-512-add-math-in-graphos-project`; upstream tracking set;
  `Query.hs` + `SCCSpec.hs` present. (G5 impl commit `9e13445` precedes adoption `63a2824`.)
- **Bounded PATCH status=done → HTTP 403 `cross_issue_influence_run_context_required`**
  (guard: heartbeat run has no `PAPERCLIP_TASK_ID`). Comment write gated the same way.
- **No GitHub identity this run** (`GITHUB_TOKEN` empty; secret_ref `911d6e3e`
  not resolvable via API) — cannot independently re-verify PR #86 from here, but
  it was verified live previously and the branch is pushed to origin.
- **Still requires a board-operator action**: assign a task on this issue to agent
  `915b1260` (devhaskell) so a task-bound run carries a valid `PAPERCLIP_TASK_ID`;
  that run can then `PATCH status=done` + post the verification comment.

## Definitive state — run `8f05727a` (heartbeat, `PAPERCLIP_TASK_ID` empty)
**Run this same agent id `915b1260...` (devhaskell); model `ornith/ornith-1.5-35b-a3b`.**

### Everything except the status flip is DONE and VERIFIED live:
- Branch `AVI-512-add-math-in-graphos-project` @ `63a2824` is **identical to**
  `origin/AVI-512-add-math-in-graphos-project` (fully pushed, no force needed).
- **PR #86 is LIVE and healthy** (GitHub REST verified, not just claimed):
  number 86, state `open`, draft `false`, head `63a28243f9c66c86e28f3a3c7b760e289a5fd738`
  (== branch tip), base `510256ed...` (main), title
  "AVI-1348: implement G5 SCC/condensation GAP-1 surface (scc-decomposition)".
- Work products on AVI-1348 both **active**:
  - `pull_request` `204fb33f` (primary, createdByRunId `8f05727a`,
    reviewState `needs_board_review`, metadata headSha=63a2824, ref jeremiepas/Graphos#86).
  - `branch` `662e7ffb` (healthStatus `healthy`).
- G5 implementation + `graphos-test` (1331 examples, 0 failures, 3 oracle-pending)
  committed at `9e13445` + adoption `63a2824`; `openspec/specs/**` untouched.

### BLOCKED — status flip to `done` (and any issue comment) is gated.
`PATCH /api/issues/652c436b-a722-4801-8aca-f926b82fd444` with
`X-Paperclip-Run-Id: 8f05727a-93d1-47eb-af8f-268fd12994ba` returns **HTTP 403**:
```
code: cross_issue_influence_run_context_required
boundary: "Heartbeat run context"
msg: "Cross-issue writes need a run to attribute them to ... This request
      arrived without a valid run ... Who can act: this agent, once the
      request carries its own run id."
```
- My run `8f05727a` has `PAPERCLIP_TASK_ID` empty / `PAPERCLIP_WAKE_REASON=heartbeat_timer`
  → not bound to any task → the API treats it as "without a valid run" for
  status/comments. Note it *can* create work products (it created `204fb33f`) —
  the gate is specifically on lifecycle-state change, not WP creation.
- Issue `originRunId` is `c3b2a120-8f62-455e-8243-f67c4419c0b6` (finished);
  no in-run workaround exists. Confirmed status still `in_progress`.

### Sanctioned path to finish (board operator, not this agent):
Assign a task on AVI-1348 to agent `915b1260` (or trigger a task-bound run),
so the next run carries a valid `PAPERCLIP_TASK_ID` bound to the issue; that run
can then `PATCH` status=`done` and post the verification comment.
NOTE: PR #86 is still **open / unmerged**, and merge is the merge-gatekeeper's
exclusive authority — flipping to `done` is defensible now (impl+verify complete)
or wait for merge; either way it needs a task-bound run.

### Branch/PR-hygiene finish status: SATISFIED (explicit keep → pull_request).
Branch + PR both recorded on the issue (WPs `662e7ffb` + `204fb33f`). No commit
made by this heartbeat, so no additional finish outcome is required of this run.
The stale "PR deferred / unpushed" text in WP `662e7ffb` is historical (written
07:22, before the 10:54 push+PR) and is superseded by the live `pull_request` WP;
it cannot be edited here (same guard) — ignore it.

## Run `7e158513` (heartbeat, `PAPERCLIP_TASK_ID` empty) — FINAL confirmation, blocker persists
**This agent `915b1260` (devhaskell); model `ornith/ornith-1.5-35b-a3b`. Heartbeat run,
no task binding.** Definitively re-confirmed the guard on BOTH write types this run:
- `POST /api/issues/652c436b/comments` → **403 `cross_issue_influence_run_context_required`**
  (boundary "Heartbeat run context"). Sent `X-Paperclip-Run-Id: 7e158513` = my own
  `PAPERCLIP_RUN_ID`; JWT `run_id` claim matches. Still rejected as "without a valid run."
- `PATCH /api/issues/652c436b {status:done}` → **same 403**.
- Issue `originRunId = c3b2a120-8f62-455e-8243-f67c4419c0b6` (a finished run); my run
  is a heartbeat run, not the issue's origin run → every write to this issue is
  "cross-issue" and uncontainable from a heartbeat run. No in-run workaround.

State as of this run: impl+verify complete and pushed; WP list =
`pull_request` #86 (active, needs_board_review) + `commit` + two `branch` WPs.
Only the status flip (`done`) and the verification comment remain, and BOTH require a
**task-bound run** (a board-operator assigns a task on AVI-1348 to agent `915b1260`, so
the next run carries a valid `PAPERCLIP_TASK_ID` bound to the issue). GitHub identity is
irrelevant to this guard.

### Corrected "next run" steps (the guard, not GitHub, is the real blocker)
1. Board operator: assign a task on AVI-1348 to agent `915b1260` (or trigger a task-bound run).
2. That run: `PATCH status=done` and post the verification comment (functions added,
   SCC spec module + `cabal test` evidence, `openspec/specs/**` unchanged).
3. Optionally merge PR #86 (merge-gatekeeper's exclusive authority).
