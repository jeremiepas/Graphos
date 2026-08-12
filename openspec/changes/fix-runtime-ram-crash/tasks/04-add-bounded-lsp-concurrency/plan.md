<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

# Task 4 — Add bounded LSP concurrency — PLAN

**Task slug**: `04-add-bounded-lsp-concurrency`
**Attempt**: 1
**Status**: pending

## Summary

Replace `mapConcurrently` for LSP file groups with a bounded semaphore pool (default 2 concurrent LSP processes). Add `--lsp-concurrency` CLI flag to `app/Main.hs`. Each group connects, extracts, and calls `disconnectLSP` before the next group starts.

## Detail

### Scope

This task modifies:
- `app/Main.hs` — add `--lsp-concurrency N` CLI flag (default 2), pass to `PipelineConfig`
- `src/Graphos/UseCase/Extract.hs` — replace `mapConcurrently (extractGroup ...)` with bounded pool using `QSemN` or `withPool`. Ensure `disconnectLSP` is called in a `bracket` after each group completes. Update `extractAll` to respect the concurrency limit.

The key behavioral change: LSP servers are no longer spawned concurrently across language groups. Instead, at most N groups run concurrently. Within each group, existing parallelism (`--threads`) is preserved.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `extraction/scen:concurrent-lsp-extraction-cap` | `specs/extraction/spec.md` | At most N LSP processes alive simultaneously on 5-language codebase |
| `extraction/scen:lsp-server-lifecycle` | `specs/extraction/spec.md` | `disconnectLSP` called when group extraction completes |
| `extraction/scen:configurable-concurrency` | `specs/extraction/spec.md` | `--lsp-concurrency 4` limits to 4; default is 2 |

**Specific tests/gates:**

1. **Integration test**: Run `graphos .` on a multi-language codebase (3+ languages) — verify via `ps aux | grep language-server` (or equivalent) that at most N LSP processes are alive at any time (N = `--lsp-concurrency` value).
2. **Regression test**: Total node/edge counts match pre-change baseline.
3. **Build gate**: `cabal test` passes with exit code 0.

**PASS conditions:**
- On a 5-language codebase, `ps aux | grep -c language-server` never exceeds N (default 2)
- Each LSP process terminates after its file group extraction (no orphaned processes)
- `--lsp-concurrency 4` correctly raises the limit to 4
- Default (no flag) limits to 2 concurrent LSP servers
- Output node/edge counts match pre-change baseline
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If more than N LSP processes are alive simultaneously, the semaphore/bound is not working correctly — check that `QSemN` acquire/release is balanced
- If extraction throughput on a single-language codebase regresses >20%, the concurrency cap is too aggressive — increase default from 2 to 3
- If LSP processes don't terminate after extraction, `disconnectLSP` is not being called — verify `bracket` pattern

### Affected Modules

- `app/Main.hs` — new `--lsp-concurrency` CLI flag, pass to pipeline config
- `src/Graphos/UseCase/Extract.hs` — bounded pool for LSP groups, `disconnectLSP` in `bracket`

### Prerequisites

- Task 1 (RTS profiling flags) should be implemented first — enables memory measurement
- Task 2 (Map accumulators) and Task 3 (batch merge) are independent but complementary
- `disconnectLSP` function exists in `Infrastructure.LSP.Client`

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Sequential LSP extraction slower for single-language codebases | Wall-clock time increase | Default cap of 2 allows within-language parallelism; increase to 3 if regression >20% |
| `QSemN` acquisition may deadlock if `release` not called on error | Process leaks | Use `bracket` pattern for guaranteed release |
| Multi-language extraction slower | Throughput decrease | Net effect may be improved due to less GC pressure — measure |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
