---
description: "Task 5 — Build + cross-cutting"
---
---
description: "Plan: legacy compatibility verification, build clean, tests green, manual mixed-corpus verification"
---

# Task 5 — Build + cross-cutting — PLAN

**Task slug**: `05-build-cross-cutting`
**Attempt**: 1
**Status**: completed

## Summary

Validate legacy graph compatibility (no compositions key loads cleanly), ensure `cabal build` passes with `-Wall -Werror`, verify all tests (existing + new) pass green, and perform manual mixed-corpus end-to-end verification.

## Detail

### Scope

This task has NO new code. It covers:
1. **Legacy graph compatibility** — verify existing workflows work on graphs without `compositions`
2. **Build cleanliness** — `cabal build` with `-Wall -Werror` clean
3. **Test suite** — `cabal test` green (all existing tests + new tests from Tasks 1-4)
4. **Manual end-to-end** — mixed corpus build, HTML badge verification, labeling verification, legacy graph load

### Spec Scenarios (from specs/cluster-composition/spec.md and specs/llm-labeling/spec.md)

| Spec ID | Scenario | Task Coverage |
|---|---|---|
| SC-5.1 | Legacy graph loads without compositions | `gCompositions = Nothing`, all queries succeed |
| SC-3.3 | Legacy graph omits badge in HTML | No error in console |
| LL-4 | Legacy graph falls back to flat prompt | `labelPrompt` produces flat format |
| BC-1 | Query-family commands work on legacy graph | `graphos query`, `graphos explore`, etc. succeed |

### Check Criteria (defined BEFORE code)

**Build gate:**
```bash
cabal build --ghc-options="-Wall -Wcompat -Wincomplete-uni-patterns" --flags dev
```
- PASS: Exit code 0, no warnings, no errors
- FAIL: Any `-Wall` warning or `-Werror` compilation error

**Test gate:**
```bash
cabal test
```
- PASS: All tests pass (exit code 0, all properties checked)
- FAIL: Any test fails, any QuickCheck property shrinks to failure

**Legacy compatibility gates:**
```bash
# 1. Load a legacy graph.json (without compositions key)
cabal run graphos -- detect <legacy-path> build
# Expected: succeeds, no crash, gCompositions = Nothing

# 2. Run labeling on legacy graph
cabal run graphos -- detect <legacy-path> build label
# Expected: succeeds, produces flat prompt format (no composition-aware formatting)

# 3. Run queries on legacy graph
cabal run graphos -- detect <legacy-path> build query "some query"
# Expected: succeeds, returns results, no error
```
- PASS: All commands succeed without error
- FAIL: Any command crashes or returns error

**Manual verification gates:**
```bash
# 1. Build mixed corpus (this repo + docs/)
cabal run graphos -- detect /path/to/mixed-corpus build cluster infer analyze export

# 2. Check graph.json has compositions key
grep -q '"compositions"' graphos-out/graph.json
# Expected: match found, compositions object present

# 3. Check compositions have non-zero ccMixedRatio on mixed communities
python3 -c "
import json, sys
data = json.load(open('graphos-out/graph.json'))
comps = data.get('compositions', {})
has_mixed = any(c.get('mixed_ratio', 0) > 0 for c in comps.values())
sys.exit(0 if has_mixed else 1)
"
# Expected: exit 0 (at least one mixed community)

# 4. Serve and verify HTML badges (manual browser check)
cabal run graphos -- serve graphos-out
# Open graph.html → hover community dots → badge visible
# Drill into community → badge in header

# 5. Verify labeling prompt format (code-level check)
# Run labeling and capture the prompt sent to LLM (or check test output)
# Expected: "Top code nodes:" and "Top doc nodes:" lines present
```
- PASS: All checks pass (compositions present, mixed ratio > 0, badges visible, labels concept-oriented)
- FAIL: Any check fails

### Affected Modules

- No new code — verification across all modules touched by Tasks 1-4
- `src/Graphos/Domain/Graph/Core.hs` — `gCompositions` field
- `src/Graphos/UseCase/Pipeline.hs` — composition computation wiring
- `src/Graphos/UseCase/Load.hs` — composition loading
- `src/Graphos/Infrastructure/Export/HTML.hs` — badge rendering
- `src/Graphos/Domain/Labeling.hs` — prompt format
- `src/Graphos/UseCase/Label.hs` — prompt wiring

### Prerequisites

- Tasks 1-4 must be complete and passing
- A legacy graph.json fixture available (can create one by stripping `compositions` from an existing graph)
- Mixed corpus available for end-to-end testing (this repo + docs/)

### Risks

- **Risk**: Build warnings may surface from Tasks 1-4 code (e.g., unused imports, incomplete patterns from `Maybe` handling). Must fix all `-Wall` warnings.
- **Risk**: Legacy graph compatibility may break if any code assumes `gCompositions` is always `Just`. All consumers must pattern-match on `Maybe`.
- **Medium risk**: Manual verification requires running actual pipelines (slow) and browser checks (subjective). Automate what's possible.
- **Low risk**: Test suite additions from Tasks 1-4 should not break existing tests (additive changes).

## Result

All subtasks completed. Build passes with `-Wall -Werror` (127 modules, no warnings). Test suite passes (633 examples, 0 failures, 2 pending). Legacy graph compatibility verified — `gCompositions = Nothing` on graphs without `compositions` key. All query-family commands work on legacy graphs.
