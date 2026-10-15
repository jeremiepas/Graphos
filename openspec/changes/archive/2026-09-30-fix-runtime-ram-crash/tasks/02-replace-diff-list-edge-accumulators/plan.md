# Task 2 — Replace diff-list edge accumulators with Map — PLAN

**Task slug**: `02-replace-diff-list-edge-accumulators`
**Attempt**: 1
**Status**: pending

## Summary

Replace all `IORef ([Edge] -> [Edge])` diff-list accumulators with `IORef (Map EdgeId Edge)` in `UseCase.Extract`, enabling incremental GC of processed batches and eliminating O(n) closure chain overhead.

## Detail

### Scope

This task modifies `src/Graphos/UseCase/Extract.hs` to change edge accumulator types:
- `codeEdgeAccRef :: IORef ([Edge] -> [Edge])` → `IORef (Map EdgeId Edge)`
- `docEdgeAccRef :: IORef ([Edge] -> [Edge])` → `IORef (Map EdgeId Edge)`
- `officeEdgeAccRef :: IORef ([Edge] -> [Edge])` → `IORef (Map EdgeId Edge)`
- `imageEdgeAccRef :: IORef ([Edge] -> [Edge])` → `IORef (Map EdgeId Edge)`

Replace diff-list append `acc . (edges ++)` with `Map.union`. Update `accumulateEdges` to read `Map EdgeId Edge` directly. Ensure `EdgeId` is properly derived for all edges.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `streaming-extraction/scen:edge-accumulator-deduplicates` | `specs/streaming-extraction/spec.md` | Map.union merge keeps second edge (right-biased), deduplicating by EdgeId |
| `streaming-extraction/scen:edge-accumulator-enables-gc` | `specs/streaming-extraction/spec.md` | Batch's local Extraction becomes unreachable after merge; Map accumulator holds no references to intermediate data |

**Specific tests/gates:**

1. **Static analysis**: `grep -r "Edge -> \\[Edge\\]" src/Graphos/UseCase/Extract.hs` returns zero matches — no diff-list type remains.
2. **Unit test**: Add or update Hspec tests verifying that `Map.union` correctly deduplicates edges by EdgeId (right-bias: newer edge wins on conflict).
3. **Regression test**: Run `graphos .` on a test codebase — node and edge counts match pre-change output exactly (same number of nodes, same number of edges).
4. **Build gate**: `cabal test` passes with exit code 0.

**PASS conditions:**
- Zero occurrences of `[Edge] -> [Edge]` (diff-list pattern) in `UseCase/Extract.hs`
- All existing edge-related tests pass
- Output node/edge counts match pre-change baseline exactly
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If edge counts differ from baseline, the diff is NOT a "minor optimization" — it indicates either lost edges (wrong) or deduplication that removed valid duplicates (also wrong). Must investigate which edges differ.
- `Map.union` right-bias means the second edge wins on conflict — if the intended behavior was "first edge wins" (preserves earliest extraction), this is a behavioral change that must be documented and accepted.

### Affected Modules

- `src/Graphos/UseCase/Extract.hs` — edge accumulator types and merge logic
- Indirect: any module that reads the accumulator IORefs (likely only `UseCase.Extract` itself)

### Prerequisites

- Task 1 (RTS profiling flags) is independent — no ordering requirement
- `EdgeId` must be well-defined and unique across all extraction categories
- Existing tests cover basic extraction correctness (node count, edge count)

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| `Map.union` may be slower for small batches | Minimal — only affects small-scale extraction | Benchmark if concerned; Map.union is O(n+m) amortized |
| Right-bias deduplication changes edge semantics | Behavioral change | Verify that duplicate EdgeIds are expected (same source file producing same edge) |
| Missing EdgeId derivation for some edge types | Compilation error or runtime duplicate edges | GHC type checker catches missing derivations |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
