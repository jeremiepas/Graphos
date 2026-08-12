# Task 1 — Fix EdgeId deduplication in production code — PLAN

**Task slug**: `01-fix-edgedid-deduplication`
**Attempt**: 1
**Status**: pending

## Summary

Replace all `EdgeId ""` occurrences in production edge-creation code with deterministic composite keys derived from source node, target node, and relation type. This eliminates silent edge deduplication caused by `Map.fromList` dropping duplicate keys.

## Detail

### Scope

- **Files to modify**: `src/Infrastructure/LSP/Extraction.hs` (`symbolTreeToEdges`, `makeEdge`), any other `src/` files that construct `EdgeId` with empty string `""`.
- **Change**: Replace `EdgeId ""` with `EdgeId (source <> "->" <> target <> ":" <> relationToText relation)` at every edge-creation point.
- **No code changes in Domain or UseCase layers** — edge ID construction is purely an Infrastructure concern.

### Check Criteria

**What tests/gates will be run:**
- `cabal build` — zero warnings (with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror` via `--flag dev`)
- `cabal test` — all tests pass (90/90)
- `grep -r 'EdgeId ""' src/` — returns zero matches

**What spec scenarios this task must satisfy:**
- `lsp-edge-extraction` spec — **"Unique EdgeId for every edge"**: Given two Contains edges from different parent symbols to the same child, each SHALL have a unique EdgeId based on source, target, and relation.

**What the exact PASS conditions are:**
1. `cabal build` exits with code 0 and zero warning lines
2. `cabal test` exits with code 0 and reports all 90 tests passing
3. `grep -r 'EdgeId ""' src/` returns exit code 1 (no matches)
4. Every edge in production code has EdgeId format: `"source->target:relation"` (non-empty)

**What would constitute a FAIL:**
- `cabal test` fails because existing tests expect `EdgeId ""` behavior (e.g., test helpers that create edges with empty IDs)
- `grep` still finds `EdgeId ""` in any file under `src/` after the change
- Build succeeds but test count decreases (tests silently passing with different behavior)

### Affected Modules

| Module | Layer |
|--------|-------|
| `Infrastructure.LSP.Extraction` | Infrastructure — `symbolTreeToEdges`, `makeEdge`, all edge-creation functions |
| `Tests` | Tests — any test helper creating `EdgeId ""` |

### Prerequisites

- All edge-creation points identified via `grep -r 'EdgeId ""' src/`
- No parallel tasks modifying the same files
- Build and test commands verified working before changes

### Risks

| Risk | Mitigation |
|------|------------|
| Test helpers using `EdgeId ""` may need updating | Update test smart constructors alongside production code |
| EdgeId collision impossible by definition (source+target+relation is unique) | No risk — deterministic composite key |
| Regression in edge count (fewer edges due to dedup) | Expected: more edges survive dedup, count should increase |

## Result

Pending — first cycle.
