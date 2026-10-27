# AVI-1436 — Route 1 (G5 SCC/condensation GAP-1) verification notes

## This run — `17d23e19` (devhaskell, task-bound assignment)

The G5 SCC/condensation GAP-1 surface was **already implemented** on branch
`AVI-512-add-math-in-graphos-project` @ `9e13445` (commit
"AVI-1348: implement G5 SCC/condensation GAP-1 surface (specs unchanged)"), with
the branch tip at `b324895` ("AVI-1348: revert ad-hoc hasCycleUndirected spec
block", which restores `spec.md` to 99 lines). This run re-verified the
deliverable from worktree `/home/jeremie/Documents/Graphos/.worktrees/AVI-512-add-math-in-graphos-project`
(head `b324895`, branch in sync with `origin` — `0 0` vs upstream).

### Verified (this run)
- `cabal build all` → **BUILD_EXIT=0** (GHC 9.10.3, lib + `openspec-view` +
  `graphos` exe + both test suites).
- `cabal test --enable-tests`:
  - `graphos-test` → **PASS: 1331 examples, 0 failures, 3 pending** (3 pending
    are real-corpus oracle skips, unrelated to this issue).
  - `graphos-test-focus` → **PASS**.
- All G5 property tests present and passing: SCC §8 golden
  `{a,b,c}/{d,e}/{f}`; condensation chain `{a,b,c}→{d,e}→{f}` (Thm 2.3);
  INV-7 (partition), INV-10 (coarsening), INV-C5 directed acyclicity
  (`isAcyclicDirected`, self-loop F3), INV-C3 isolated nodes (`isolatedNodes`),
  undirected cycle detection (`hasCycleUndirected`, INV-C2), connectivity
  decomposition (`connectedComponents`/`weaklyConnectedComponents`, INV-C1/C6).

### Surface (Graphos.Domain.Graph.Query, re-exported from Graphos.Domain.Graph)
- `cond`, `isAcyclicDirected`, `connectedComponents`, `weaklyConnectedComponents`,
  `isolatedNodes`, `hasCycleUndirected` + `*WithCached` variants (Query.hs);
  re-exports in Graph.hs.

### Spec — unchanged
- `openspec/specs/scc-decomposition/spec.md` = **99 lines**, byte-identical to
  `origin/main` (empty `git diff origin/main -- .../spec.md`). Branch tip `b324895`
  is the revert of the `63a2824` ad-hoc spec block that would otherwise have
  polluted the spec. No requirement edits.

## Mandatory finish step — satisfied
1. **Explicit keep (branch WP)** — `branch` work product
   `a9240a52` (status `active`, created by origin run `204b23b2`) is recorded on
   this issue. It names branch `AVI-512-add-math-in-graphos-project` (tip
   `b324895`, not merged into origin/main), records implementation + verification,
   and explicitly defers the PR ("GitHub was unavailable this run"). This is the
   AGENTS.md mandatory finish; no duplicate WP.
2. **Verification/disposition comment** — posted by this run
   (`10225732`), confirming build + tests (1331 examples, 0 failures, 3 pending)
   and spec-unchanged.

## Notes for future runs
- This run (`17d23e19`) is `invocationSource: assignment` with a populated
  `PAPERCLIP_TASK_ID`, so write access to this issue worked (comment posted).
  The systemic 403 guard from `stop-gate-2026-10-23.md` applied to *unassigned*
  heartbeats (empty task ID); it does not apply to a task-bound assignment run.
- PR remains deliberately deferred: no managed GitHub identity this run. The
  merge-gatekeeper owns opening the PR once GitHub access returns.
