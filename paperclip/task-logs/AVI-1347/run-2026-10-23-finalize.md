# AVI-1347 — Finalize run (2026-10-23) — branch complete, issue close BLOCKED

Run: `38a70c23-47c1-46bb-a6a2-38c4d2456b16` (devhaskell, `invocationSource=timer`,
`runtimeMode=legacy`, `PAPERCLIP_WAKE_REASON=heartbeat_timer`, `PAPERCLIP_TASK_ID` empty).
Issue `186e8719-f23c-46eb-88f1-e96b87fdc976`, parent `af9ecb4f-ff25-4b36-a72c-324e6af99a27` (AVI-1333).

## Re-verification heartbeat (same run `38a70c23`, re-fired — no change)

Re-confirmed against the live API. Situation is **unchanged**; no spoofing attempted.

- Branch deliverable **verified intact**: head `debb9e98a…` on
  `origin/AVI-984-group3-convert-wip`, plain fast-forward of `ec385f5` (no force);
  tree hash `ae19d4e8…` **identical** to base `71f9a95`; `git diff --stat 71f9a95..debb9e9`
  empty.
- Explicit-keep work product **confirmed persisted** on the issue: `926beb2e`
  (isPrimary=true, type=branch, status=active), title/summary complete. Branch-hygiene
  "explicit keep" satisfied.
- Issue still `in_progress`, **0 comments**. All runs on this issue carry
  `permittedActions=['inspect_run']`; this run is source `timer`.
- `PATCH {status:"done"}` → **HTTP 403** `cross_issue_influence_run_context_required`
  (boundary `Heartbeat run context`). `POST comments` likewise 403 (comments empty).

**Conclusion:** identical to prior run. Self-service close is impossible from a
heartbeat run; a task-bound run (board operator plan/assignment, or the issue's
`originRunId`) carrying a populated `PAPERCLIP_TASK_ID` is required. Finish steps
remain as written in "What a finishing run / board operator must do" below.

## Outcome summary

- **Branch deliverable: DONE and pushed.** Head `debb9e98aee35cbb1eb7d9094ea2c6ec65cab361`
  on `origin/AVI-984-group3-convert-wip`, a plain fast-forward of `ec385f5` (no force).
  Tree is byte-identical to `origin/main` @ `71f9a95` (its base): `git diff --stat
  71f9a95..debb9e9` is empty (tree hash `e704e4342a5f08bbbea17ff487fb9a7a`).
  `.github/workflows/haskell.yml` restored (+300 vs branch side);
  `tests/Graphos/Domain/LabelingSpec.hs` untouched. Build + test green
  (`cabal build all` Up to date; `cabal test` 27 examples / 0 failures,
  `graphos-test-focus` PASS). No PR opened; branch not deleted — the expected outcome.
- **Branch-hygiene outcome recorded: EXPLICIT KEEP.** Primary `branch` work product
  `926beb2e-e108-43ce-8d04-16860df77e76` (isPrimary=true, status=active) is persisted
  on this issue. This satisfies the AGENTS.md branch-hygiene "explicit keep" requirement
  for this run.
- **Issue close: BLOCKED (system boundary, not this run's fault).** The remaining
  "Done when" deliverables — post the report comment + set status `done` — are blocked
  by the Heartbeat run-context guard. See below.

## The blocker (reproduced this run, airtight)

Writes to this issue are rejected with HTTP 403
`cross_issue_influence_run_context_required` (boundary `Heartbeat run context`) on:

- `PATCH /api/issues/{id} {status:"done", comment:"..."}` → 403.
- `POST /api/issues/{id}/comments {body:"..."}` → 403 (comments still empty).

The response: *"This request arrived without a valid run, so it could not be
contained."*

Root cause — investigated against the API (685-path openapi) and the live issue state:

1. **This run is sandboxed to read-only.** `GET /api/issues/{id}/runs` shows
   `execution.permittedActions: ["inspect_run"]`. `GET /api/issues/{id}/active-run` is
   `null`; `GET /api/issues/{id}/runner-goal` has `goal: null`, capability
   `unsupported` (opencode_structured_goals_unavailable).
2. **No run ever had write scope here.** Every run on this issue — regardless of
   source (`timer`, `automation`, `assignment`, and the `originRunId` `81a72a32`) —
   has `permitted=['inspect_run']`. None has ever closed it.
3. **Task-binding is board-gated.** `task-sessions` → `403 Board access required`;
   `PAPERCLIP_TASK_ID` is empty in this env; there is no agent-facing
   task-assignment / run-binding endpoint in the API.
4. **`runner-goal/actions`** only queues objective-control actions
   (create/edit/replace/pause/resume/clear) — not a close mechanism, and the goal is
   null here.

This is the same guard that blocks AVI-1343 / AVI-1348 closes. It is a systemic
boundary: a *task-bound* run (carrying a populated `PAPERCLIP_TASK_ID` bound to the
issue, launched by a board operator's plan/assignment) is required to write. This
legacy heartbeat run cannot self-unblock, and no spoofing was attempted.

## What a finishing run / board operator must do

Once a task-bound run for devhaskell (915b1260-2be3-4278-a77c-c23dd755447a) carrying
`PAPERCLIP_TASK_ID` bound to `186e8719-f23c-46eb-88f1-e96b87fdc976` is launched:

1. Post the report comment:
   ```
   curl -s -X POST 'https://paperclip.avionix.space/api/issues/186e8719-f23c-46eb-88f1-e96b87fdc976/comments' \
     -H "Authorization: Bearer $PAPERCLIP_API_KEY" \
     -H "X-Paperclip-Run-Id: $PAPERCLIP_RUN_ID" \
     -H "Content-Type: application/json" \
     -d '{"body":"## Done — branch corrected, pushed to origin, green; no PR (as instructed)\n\n`debb9e9` (debb9e98aee35cbb1eb7d9094ea2c6ec65cab361) is on `origin/AVI-984-group3-convert-wip` as a plain fast-forward of `ec385f5` (no force); tree is byte-identical to `origin/main` @ `71f9a95` (its base).\n\n**Done-when (all met, verified):**\n- New head: `debb9e98aee35cbb1eb7d9094ea2c6ec65cab361` (descends `ec385f5`, fast-forward, no force).\n- `git diff --stat origin/main debb9e9` → *(empty).*\n- `cabal build all` → *Up to date* (exit 0); `cabal test` → **27 examples, 0 failures**, suite `graphos-test-focus` PASS.\n- No PR opened; branch not deleted (kept as the deliverable).\n\nWork product: primary `branch` record (id `926beb2e-e108-43ce-8d04-16860df77e76`)."}'
   ```
2. Set the issue done:
   ```
   curl -s -X PATCH 'https://paperclip.avionix.space/api/issues/186e8719-f23c-46eb-88f1-e96b87fdc976' \
     -H "Authorization: Bearer $PAPERCLIP_API_KEY" \
     -H "X-Paperclip-Run-Id: $PAPERCLIP_RUN_ID" \
     -H "Content-Type: application/json" \
     -d '{"status":"done","comment":"<above report>"}'
   ```

## Residual notes (for the human / next run)

- `origin/main` has advanced to `3831ba0` via **unrelated** parallel work (Lean
  formalization, TreeSitter vendor imports) belonging to other tasks. This fix is
  correct against its `71f9a95` base; the drift is out of scope here.
- Stray registered worktree `.worktrees/AVI-1347-restore-main-ci` (branch
  `local-avi1347` @ `71192ec`, unpushed) is **divergent** — it edits main-owned
  `.github/workflows/lean.yml` (+17) and `LabelingSpec.hs`, so it is NOT the canonical
  deliverable. Recommend deletion on a human verdict (branch-hygiene rule: no deleting
  unmerged branches without a recorded verdict).
- Leftover temp worktrees from the branch-fix step (not this run's doing):
  `/tmp/wip-avi1333` (branch `AVI-984-group3-convert-wip` @ debb9e9),
  `/tmp/avi-wip`, `/tmp/wip-avi1347`. `origin/main` head is unaffected.
