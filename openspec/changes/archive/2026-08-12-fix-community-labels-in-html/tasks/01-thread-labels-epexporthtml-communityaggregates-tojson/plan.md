<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Thread labels through `epExportHTML` → `communityAggregatesToJSON` — PLAN

**Task slug**: `01-thread-labels-epexporthtml-communityaggregates-tojson`
**Attempt**: 1
**Status**: pending

## Summary

Extend the HTML export pipeline to carry LLM-generated community labels from `exportAll` through `epExportHTML` into `communityAggregatesToJSON`, so the HTML viewer displays LLM labels (with `"Community <id>"` fallback) instead of the hardcoded fallback everywhere.

## Detail

### Scope

Modify four files to thread `Maybe (Map CommunityId Text)` from the UseCase layer into the HTML export function:

1. **`src/Graphos/UseCase/Port/ExportPort.hs`** — Add `Maybe (Map CommunityId Text)` as the third parameter to `epExportHTML`: change from `Graph -> Analysis -> FilePath -> IO ()` to `Graph -> Analysis -> Maybe (Map CommunityId Text) -> FilePath -> IO ()`.
2. **`src/Graphos/Infrastructure/Export/HTML.hs`** — Update `exportHTML` and `communityAggregatesToJSON` to accept the labels parameter. In `communityAggregatesToJSON`, replace the hardcoded `vcaLabel = T.pack ("Community " ++ show cid)` with a label lookup: `maybe ("Community " ++ show cid) id (mLabels >>= Map.lookup cid)`. Also update the overview dot `title` to include the LLM label.
3. **`src/Graphos/Infrastructure/Wiring.hs`** — Update the single `epExportHTML` wiring site (line ~204) to pass the labels through.
4. **`src/Graphos/UseCase/Export.hs`** — In `exportAll` (line ~37), pass `mLabels` to the `epExportHTML` call instead of dropping it.

### Check Criteria (defined BEFORE code)

**Spec scenarios satisfied:**
- `html-lod-viewer` — "Aggregate fields populated with LLM label" (label = LLM value, not placeholder)
- `html-lod-viewer` — "Fallback when no labels provided" (label = `"Community <id>"` when `Nothing`)
- `html-lod-viewer` — "HTML viewer shows label in sidebar" (embedded `_communityAggregatesData` contains LLM labels)
- `llm-labeling` — "No labeling config in graphos.yaml" (defaults apply, no crash)
- `llm-labeling` — "Explicit OpenAI config still works" (labels flow through correctly)

**Tests/gates to run:**
- `cabal build --flag dev` — exit code 0, zero warnings
- `cabal test` — all examples pass (currently 347+ examples)
- New Hspec tests in `Graphos.Infrastructure.Export.HTMLSpec`:
  - `testLabelPresent` — `communityAggregatesToJSON` with `Just (Map.fromList [(4, "Authentication Module")])` produces `vcaLabel = "Authentication Module"` for community 4
  - `testLabelAbsent` — `communityAggregatesToJSON` with `Nothing` produces `vcaLabel = "Community 7"` for community 7
  - `testLabelPartial` — `communityAggregatesToJSON` with `Just (Map.fromList [(4, "X")])` produces `vcaLabel = "Community 7"` for community 7 (fallback)
  - `testLabelEmptyString` — `communityAggregatesToJSON` with `Just (Map.fromList [(4, "")])` treats empty string as absent, produces `vcaLabel = "Community 4"` for community 4
- New Hspec test in `Graphos.UseCase.ExportSpec`: stub `ExportPort` records that `epExportHTML` was called with `mLabels` argument

**PASS conditions:**
- `cabal build --flag dev` exits 0 with zero warnings (no `-Wall`, `-Wcompat`, `-Wincomplete-uni-patterns`, or `-Werror` failures)
- `cabal test` exits 0, all examples pass
- All four `HTMLSpec` tests pass with the exact expected label values
- `ExportSpec` stub test verifies label threading
- No changes to `graph.json` schema (the `community_labels` key is written by a separate module)

**FAIL boundaries:**
- Build fails due to unhandled unlifted type from the port signature change
- `Wiring.hs` has additional `epExportHTML` call sites not found during initial grep (the design says there is exactly one, but if there are more, they must all be updated)
- `communityAggregatesToJSON` test produces `vcaLabel = "Community 4"` even when the label map contains `4 -> "Authentication Module"` (fallback not overridden)
- Empty string `""` in the label map is treated as a valid label instead of falling back to `"Community <id>"`

### Affected modules

| File | Change type | Risk |
|------|-------------|------|
| `UseCase/Port/ExportPort.hs` | Signature change | Low — one call site in Wiring.hs |
| `Infrastructure/Export/HTML.hs` | Signature + logic change | Medium — `communityAggregatesToJSON` is the core rendering function |
| `Infrastructure/Wiring.hs` | Argument pass-through | Low — mechanical change |
| `UseCase/Export.hs` | Argument pass-through | Low — mechanical change |

### Prerequisites

- GHC 9.10 with Cabal 3.0 available (via `nix-shell shell.nix`)
- Pre-existing parse error in `src/Graphos/UseCase/Extract/Core.hs:155` does NOT block this task (this task does not touch that file), but `cabal build` must eventually pass for the build gate
- `FGL 5.8+` and `Aeson 2.0+` already in use (no new dependencies needed)

### Risks

- **Port signature ripples**: The design states there is exactly one `epExportHTML` caller in Wiring.hs. If a grep reveals more, the mechanical pass-through of `Nothing` must extend to those sites before the label-using change.
- **Partial labeling**: Communities without an LLM label in the map get `"Community <id>"` via fallback — this is the expected behavior and is covered by the `testLabelPartial` test.
- **Empty-string labels**: Per spec, empty strings are treated as absent labels — the `testLabelEmptyString` test enforces this.
