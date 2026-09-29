# Task 8 — Integration test and memory profiling — PLAN

**Task slug**: `08-integration-test-and-memory-profiling`
**Attempt**: 1
**Status**: pending

## Summary

Run the full pipeline on a 50k+ file multi-language codebase with `--rts-profile --max-heap 8G` to verify peak memory < 8GB, no OOM crash, and correctness against pre-change baseline.

## Detail

### Scope

This task is an integration test and verification step — no code changes beyond what tasks 1-7 implement. It validates that all previous tasks together achieve the goal of reducing peak runtime memory from 40-60GB+ to < 8GB on large codebases.

Steps:
1. Create or find a test codebase with 50k+ files spanning at least 3 languages
2. Run `graphos . --rts-profile --max-heap 8G` on this codebase
3. Collect GC statistics from `+RTS -s` output
4. Compare output `graph.json` with pre-change baseline (structural comparison)
5. Update `.opencode/context/core/standards/code-quality.md` with memory-aware patterns (bounded buffers, incremental merge, compact types)

### Check Criteria

**Spec scenarios satisfied:**

This task validates ALL spec scenarios from tasks 1-7 collectively:

| Task | Spec Scenarios |
|------|----------------|
| Task 1 | `pipeline/scen:rtss-profiling-gc-stats`, `pipeline/scen:max-heap-limits-memory`, `pipeline/scen:flags-combined` |
| Task 2 | `streaming-extraction/scen:edge-accumulator-deduplicates`, `streaming-extraction/scen:edge-accumulator-enables-gc` |
| Task 3 | `streaming-extraction/scen:sequential-batch-merge`, `streaming-extraction/scen:memory-no-accumulate` |
| Task 4 | `extraction/scen:concurrent-lsp-extraction-cap`, `extraction/scen:lsp-server-lifecycle`, `extraction/scen:configurable-concurrency` |
| Task 5 | `graph-enrichment/scen:adding-inferred-edges`, `graph-enrichment/scen:no-intermediate-extraction`, `graph-enrichment/scen:backward-compatibility` |
| Task 6 | `bounded-observability/scen:span-eviction`, `bounded-observability/scen:default-span-capacity`, `bounded-observability/scen:histogram-aggregation`, `bounded-observability/scen:prometheus-rendering`, `bounded-observability/scen:buffer-flush` |
| Task 7 | `compact-nodes/scen:json-round-trip`, `compact-nodes/scen:memory-reduction`, `compact-nodes/scen:nodeextra-nothing`, `compact-nodes/scen:short-label-storage`, `compact-nodes/scen:long-label-fallback` |

**Specific tests/gates:**

1. **Integration test — peak memory**: Run `graphos . --rts-profile --max-heap 8G` on 50k+ file codebase — verify `+RTS -s` shows peak heap < 8GB. Extract "max bytes used" from output.
2. **Integration test — no OOM**: Process completes without OOM kill (no kernel OOM killer intervention, no GHC "Gc failed" error).
3. **Regression test — structural similarity**: Output `graph.json` is structurally similar to baseline (within 1% tolerance for community detection non-determinism). Compare: node count, edge count, community count, node fields.
4. **Build gate**: `cabal test` passes with exit code 0 (all unit/integration tests).

**PASS conditions:**
- Peak heap from `+RTS -s` < 8GB on 50k+ file codebase
- No OOM crash during full pipeline
- All 7 previous tasks pass their own individual checks (referenced in each task's plan)
- Output `graph.json` node count matches baseline within 1%
- Output `graph.json` edge count matches baseline within 1%
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If peak memory > 8GB, identify which pipeline phase causes the spike:
  - Extract phase: LSP concurrency or Map accumulation still too high
  - Build phase: `addEdges` not reducing memory
  - Cluster phase: Leiden intermediate structures still too large
  - Export phase: JSON/HTML generation memory spike
- If `cabal test` fails, determine which task's tests broke — each fix is independently testable
- If output differs from baseline by > 1%, investigate whether it's a real data loss or acceptable non-determinism (Leiden community detection)

### Affected Modules

- Integration testing only — no code changes
- `.opencode/context/core/standards/code-quality.md` — update with memory-aware patterns (after verification)

### Prerequisites

- All 7 previous tasks are implemented and individually verified
- A test codebase with 50k+ files spanning at least 3 languages is available
- Pre-change baseline output (`graph.json`) is saved for comparison

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Test codebase may not be available or too small | Can't verify peak memory target | Use largest available codebase; document actual peak memory achieved |
| Community detection non-determinism | Output differs from baseline | Use 1% tolerance for community count; verify node/edge counts exactly |
| Peak memory > 8GB on target codebase | Goal not met | Identify which phase causes spike; optimize further |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK -> "FAIL - see attempt-2/" and start a new P-D-C-A. -->
