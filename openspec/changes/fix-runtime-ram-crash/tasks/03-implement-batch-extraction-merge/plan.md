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

# Task 3 — Implement batch extraction merge with incremental GC — PLAN

**Task slug**: `03-implement-batch-extraction-merge`
**Attempt**: 1
**Status**: pending

## Summary

Restructure `extractAll` to merge extraction results batch-by-batch into a single `Extraction`, calling `evaluate` + `performGC` after each batch completes, bounding peak memory during extraction.

## Detail

### Scope

This task modifies `src/Graphos/UseCase/Extract.hs`:
- Restructure `extractAll` to process categories (code, doc, office, image) in sequence or bounded parallelism
- After each category's extraction completes, merge into the running `Extraction`
- Evaluate `Map.size` of the aggregate and call `performGC` before starting the next batch
- Remove the 8 separate IORefs (from Task 2) and use a single `IORef Extraction` or direct return values

The key behavioral change: instead of accumulating all results in IORefs and merging at the end, merge incrementally.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `streaming-extraction/scen:sequential-batch-merge` | `specs/streaming-extraction/spec.md` | After each file group, merge into running aggregate via `mergeExtractions`, evaluate size, call `performGC` |
| `streaming-extraction/scen:memory-no-accumulate` | `specs/streaming-extraction/spec.md` | Peak memory during extraction ≤ 2× final aggregate size; memory decreases after GC |

**Specific tests/gates:**

1. **Memory profiling**: Run `graphos . +RTS -s` on a 5k+ file codebase — verify peak memory is lower than pre-change baseline. Extract the "MB main memory" or "max bytes used" from `+RTS -s` output.
2. **Regression test**: Total node/edge counts in `graph.json` output match pre-change baseline (same count, same edges).
3. **Build gate**: `cabal test` passes with exit code 0.

**PASS conditions:**
- `+RTS -s` output shows peak memory significantly lower than pre-change (target: <50% reduction on large codebases)
- No OOM crashes on 5k+ file codebases
- Node count and edge count in output exactly match pre-change baseline
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If peak memory does NOT decrease, the batch merge is not effective — may need to add explicit `deepseq` or `evaluate` before `performGC`
- If node/edge counts differ from baseline by more than 0, something is lost in the merge — must identify which edges/nodes are missing
- If GC pauses cause >20% increase in extraction wall-clock time, reduce GC frequency (every N batches instead of every batch)

### Affected Modules

- `src/Graphos/UseCase/Extract.hs` — `extractAll` function, batch processing loop, GC hooks
- Indirect: `src/Graphos/Domain/Types/Extraction.hs` (merge function used for batch merge)

### Prerequisites

- Task 1 (RTS profiling flags) should be implemented first — enables memory measurement
- Task 2 (Map accumulators) should be implemented first — Map-based accumulation is the foundation for batch merge
- `mergeExtractions` function exists or is added in this task

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| GC pauses between batches may slow extraction | Wall-clock time increase | Measure first; reduce GC frequency if >20% slowdown |
| Batch merge order affects edge deduplication | Behavioral change | Right-bias Map.union is deterministic; document behavior |
| `performGC` may not reclaim expected memory | Insufficient improvement | Add explicit `evaluate` on aggregate size before GC call |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
