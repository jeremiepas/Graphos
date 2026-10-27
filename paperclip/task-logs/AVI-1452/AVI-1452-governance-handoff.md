# AVI-1452 — Governance handoff (implementation by head-of-dev)

**Issue:** `f3b7fdae-108f-4979-a373-8e91800314c0` (parent AVI-1450, done). High priority.
**Deliverable:** `AVI-1452-implementation-spec.md` (this repo, workspace root).

---

## 0. Why this handoff exists (governance gate)

`cross_issue_influence_run_context_required` is **active** in this run. The
current run token is an **unassigned heartbeat run** with **no control-plane
access** — every `https://paperclip.avionix.space/api/v1/*` route returns
`{"error":"API route not found"}` (verified in-session). Consequences:

- This run **must not** create issues, open PRs, edit tasks, comment on the
  issue, or otherwise write to the control plane.
- This run **must not** merge. Merging is the merge-gatekeeper's exclusive
  authority.
- The correct deliverable is a **precise, implementation-ready spec + this
  handoff**, written locally. The **task-bound run** (the one dispatched against
  AVI-1452) is the one that CAN reach the API and must execute the work.

## 1. Dispatch: create a child issue assigned to `devhaskell`

The task-bound run should:

1. Open an issue **under parent AVI-1452** (attach the spec artifact).
2. Set assignee = **`devhaskell`** (head-of-graphos dev cell).
3. Reference `f3b7fdae-108f-4979-a373-8e91800314c0` so the linkage is first-class.
4. Set priority to match AVI-1452 (high).

This mirrors the established child-issue → devhaskell dispatch pattern used for
`fix-buildgraph-self-edges` and Group 3 (AVI-1452) scope.

## 2. Implementation route — review gates

`devhaskell` implements `AVI-1452-implementation-spec.md`, then routes through:

```
devhaskell implements  ->  opens PR  ->  haskell-reviewer
   ->  pr-validator  ->  openspec-verifier  ->  (merge-gatekeeper)
```

- **haskell-reviewer**: confirms the `Convert.hs`/`Core.hs` changes are idiomatic
  and the spec is followed exactly (esp. the deterministic collision pass and the
  `extractionConfigFingerprint` bump).
- **pr-validator**: runs `cabal test`; asserts the five task 3.3 criteria (layout,
  `#L` suffix, determinism, no-source-text-in-id, 512-byte/nod bound) and that
  JGF contract tests still pass.
- **openspec-verifier**: confirms the spec change (`usable-graph-for-code-review`,
  task 3.3) matches the produced implementation; update the change's task status.

The merge-gatekeeper performs the merge. This run has **no** merge authority.

## 3. Branch & commit hygiene

- Branch name: `feature/AVI-1452-<slug>` (e.g. `feature/AVI-1452-bounded-node-ids`).
- **Do not** touch, commit, or switch the local uncommitted
  `feature/AVI-1447-calls-edge-emission` diff (call-edge helpers) — it is unrelated
  work and must be left as-is.
- No force-push, ever.

## 4. Finish outcomes (pick exactly one)

Per the branch/PR hygiene rule, the task-bound run ends with one recorded outcome:

1. **Push + open PR (default):** push `feature/AVI-1452-<slug>`, open the PR,
   record a `pull_request` work product on the child issue.
2. **Explicit keep (WIP only):** if stopping mid-implementation, record a `branch`
   work product naming the branch + why the PR is deferred.
3. **Explicit close:** only when the child issue is fully merged; delete the local
   branch and record the verdict. Never delete an unmerged branch without a human.

## 5. Deferred close for this issue

Once the child PR is merged by the merge-gatekeeper, close AVI-1452 and record the
outcome. This run cannot perform this step.

---

### Artifacts produced by this run (local only)
- `AVI-1452-implementation-spec.md` — full implementation spec.
- This handoff.
No control-plane writes were made.
