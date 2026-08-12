<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Add unit tests for label threading and fallback — PLAN

**Task slug**: `03-add-unit-tests-label-threading-fallback`
**Attempt**: 1
**Status**: pending

## Summary

Add Hspec tests covering label threading through the export pipeline and label fallback behavior in `communityAggregatesToJSON`, ensuring the changes from tasks 1 and 2 are verified by automated tests.

## Detail

### Scope

Create two new test modules:

1. **`test/Graphos/Infrastructure/Export/HTMLSpec.hs`** (or extend existing test file) — Add four Hspec tests for `communityAggregatesToJSON`:
   - **`testLabelPresent`**: Create a `CommunityAggregate` with `id = 4`, `memberCount = 17`. Call `communityAggregatesToJSON` with `mLabels = Just (Map.fromList [(4, "Authentication Module")])`. Assert the resulting JSON has `"label"` = `"Authentication Module"` (not `"Community 4"`).
   - **`testLabelAbsent`**: Same aggregate with `id = 7`. Call with `mLabels = Nothing`. Assert `"label"` = `"Community 7"`.
   - **`testLabelPartial`**: Aggregate with `id = 7`, labels = `Just (Map.fromList [(4, "X")])`. Assert `"label"` = `"Community 7"` (fallback for community not in map).
   - **`testLabelEmptyString`**: Aggregate with `id = 4`, labels = `Just (Map.fromList [(4, "")])`. Assert `"label"` = `"Community 4"` (empty string treated as absent).

2. **`test/Graphos/UseCase/ExportSpec.hs`** (or extend existing) — Add a stub `ExportPort` threading test:
   - Create a stub `ExportPort` that records the arguments passed to `epExportHTML`.
   - Call `exportAll` with `mLabels = Just (Map.fromList [(1, "Test")])`.
   - Assert that the stub captured the `Just` labels value passed to `epExportHTML`.

### Check Criteria (defined BEFORE code)

**Spec scenarios satisfied:**
- `html-lod-viewer` — "Aggregate fields populated with LLM label" (verified by `testLabelPresent`)
- `html-lod-viewer` — "Fallback when no labels provided" (verified by `testLabelAbsent` and `testLabelPartial`)
- `html-lod-viewer` — "HTML viewer shows label in sidebar" (verified by `testLabelPresent` — the JSON structure the sidebar reads from is correct)

**Tests/gates to run:**
- `cabal test` — all examples pass (target: 347+ examples, including new tests)
- `cabal build --flag dev` — zero warnings

**PASS conditions:**
- All four `HTMLSpec` tests pass with exact label assertions
- `ExportSpec` stub test passes (labels thread through `exportAll` → `epExportHTML`)
- `cabal test` exits 0 with all examples passing
- No existing tests are broken by the changes

**FAIL boundaries:**
- `communityAggregatesToJSON` produces `"Community 4"` instead of `"Authentication Module"` when a label is present (the fallback path is still taken)
- `exportAll` passes `Nothing` to `epExportHTML` despite receiving `Just labels` (threading broken)
- Empty string `""` is treated as a valid label (should fall back to `"Community <id>"`)
- New tests fail due to import errors or type mismatches from task 1/2 changes not being complete yet

### Affected modules

| File | Change type | Risk |
|------|-------------|------|
| `test/Graphos/Infrastructure/Export/HTMLSpec.hs` | New tests | Low — pure JSON assertion tests |
| `test/Graphos/UseCase/ExportSpec.hs` | New test | Low — stub-based test, no IO |

### Prerequisites

- Tasks 1 and 2 completed: `communityAggregatesToJSON` accepts labels parameter, `exportAll` threads `mLabels`
- `cabal build` passes (build gate must be green before tests can run)
- `Aeson` `Value` type and `Data.Map` available in test modules

### Risks

- **Test ordering dependency**: If tasks 1/2 are not complete when task 3 runs, the tests will not compile. The tasks are independent in `tasks.md` but this task logically depends on the type changes from tasks 1 and 2.
- **Stub `ExportPort` construction**: Creating a stub `ExportPort` requires knowing the full record shape. If the record has required fields beyond `epExportHTML`, the stub must supply them (possibly with `error "stub"` for unused callbacks).
- **Empty-string handling**: The spec says empty-string labels are treated as absent. The test `testLabelEmptyString` must assert that `""` falls back to `"Community <id>"`, not that it passes through as `""`.
