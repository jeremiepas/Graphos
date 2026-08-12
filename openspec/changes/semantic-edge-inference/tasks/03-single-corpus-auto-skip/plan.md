<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Single-corpus auto-skip — PLAN

**Task slug**: `03-single-corpus-auto-skip`
**Attempt**: 1
**Status**: pending

## Summary

Implement `isSingleCorpus :: Graph -> Bool` to detect graphs where all nodes share one `FileType`, enabling automatic skip of the semantic edge inference pass on homogeneous (single-corpus) graphs. Covers subtask 3.1.

## Detail

### Scope

- **Function implementation** (`src/Graphos/UseCase/Infer.hs` or `Domain/Graph/Core.hs`):
  - `isSingleCorpus :: Graph -> Bool`
  - Extracts the set of distinct `FileType` values across all nodes
  - Returns `True` when exactly one distinct `FileType` exists
  - Returns `False` when two or more distinct `FileType` values exist, or when the graph has zero nodes
  - Exported from the module for use in pipeline gating (Task 4)

- **Implementation approach**:
  - Collect all `FileType` values: `Set.map nodeFileType (Map.elems (gNodes g))`
  - Check cardinality: `Set.size == 1` → `True`, otherwise → `False`
  - Empty graph case: zero nodes → one `FileType` (or zero) — treat as single-corpus (safe skip)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in updated `InferSpec.hs`
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: Pure-code graph skips semantic inference` (spec `semantic-edge-inference` § "Pure-code graph skips semantic inference")
- `Scenario: Force flag overrides auto-skip` (spec `semantic-edge-inference` § "Force flag overrides auto-skip")

**PASS conditions**:
- `isSingleCorpus` returns `True` for a graph containing only `CodeFile` nodes
- `isSingleCorpus` returns `True` for a graph containing only `DocFile` nodes
- `isSingleCorpus` returns `True` for an empty graph (zero nodes — single or zero distinct types)
- `isSingleCorpus` returns `False` for a graph with mixed `CodeFile` and `DocFile` nodes
- `isSingleCorpus` returns `False` for a graph with `CodeFile`, `DocFile`, and any other `FileType` (e.g., `DataFile`)
- Function is efficient: O(n) where n = number of nodes (single pass over node map)
- The function is exported from the `Infer` module

**FAIL boundaries**:
- If `isSingleCorpus` returns `True` for a mixed corpus (code + docs), the test fails — this would incorrectly skip semantic inference on graphs where it's needed most
- If `isSingleCorpus` returns `False` for an empty graph, the test fails — an empty graph should be safely treated as single-corpus (no harm in skipping)
- If the function depends on IO (e.g., reading files), the test fails — it must be pure

### Affected modules

- **Modified**: `src/Graphos/UseCase/Infer.hs` — add `isSingleCorpus`, export it
- **Modified**: `test/Graphos/UseCase/InferSpec.hs` — add Hspec test cases
- **Imports from**: `src/Graphos/Domain/Graph/Core.hs` (Graph, Node, FileType), `Data.Set` (Set operations)

### Prerequisites

- `nodeFileType :: Node -> FileType` accessor exists in `Domain/Graph/Core.hs`
- `gNodes :: Map NodeId Node` accessor exists
- `FileType` data type includes `CodeFile`, `DocFile`, and potentially other constructors

### Risks

- **Low**: This is a simple pure function with no IO — minimal integration risk
- **Low**: The function follows existing patterns (similar to other `is*` predicates in the codebase)
- **Low**: No changes to existing functions — additive only
