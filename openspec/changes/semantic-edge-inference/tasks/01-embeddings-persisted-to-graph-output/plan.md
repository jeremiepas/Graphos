# Task 1 — Embeddings persisted to graph output — PLAN

**Task slug**: `01-embeddings-persisted-to-graph-output`
**Attempt**: 1
**Status**: pending

## Summary

Add embedding fields (`gEmbeddings`, `gEmbeddingsPath`) to the `Graph` type, wire serialization of `embeddings_path` in `ToJSON`/`FromJSON`, write `embeddings.json` sidecar in the pipeline when embeddings are generated, and follow the pointer in `loadGraphFromFile`. This covers subtasks 1.1, 1.2, and 1.3.

## Detail

### Scope

- **Graph type extension** (`src/Graphos/Domain/Graph/Core.hs`):
  - Add `gEmbeddings :: Maybe (Map NodeId [Double])` — embedding vectors keyed by node ID, `Nothing` when not available
  - Add `gEmbeddingsPath :: Maybe FilePath` — pointer to the sidecar file, `Nothing` when no sidecar
  - Both fields are additive with `Nothing` defaults — legacy graphs without these fields load unchanged
  - Update `ToJSON` to write `embeddings_path` when `Just` (omit when `Nothing`)
  - Update `FromJSON` to read `embeddings_path` (default `Nothing` when absent)

- **Pipeline sidecar writer** (`src/Graphos/UseCase/Pipeline/Core.hs`):
  - After building the graph, if embeddings were generated during the pipeline, write `embeddings.json` to the output directory
  - `embeddings.json` is a JSON object: `Map NodeId [Double]`
  - Set `gEmbeddingsPath = Just "embeddings.json"` on the output graph
  - If no embeddings were generated, leave `gEmbeddingsPath = Nothing`

- **Loader pointer following** (`src/Graphos/UseCase/Load.hs`):
  - In `loadGraphFromFile`: after loading `graph.json`, if `gEmbeddingsPath = Just path`, read the sidecar file and populate `gEmbeddings = Just ...`
  - If sidecar file is missing, log a warning and set `gEmbeddings = Nothing` (not an error)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in new/updated spec files for this task
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: Pipeline writes embeddings sidecar` (spec `semantic-edge-inference` § "Pipeline writes embeddings sidecar", spec `embedding` § "Pipeline writes embeddings sidecar")
- `Scenario: Legacy graph loads without embeddings` (spec `semantic-edge-inference` § "Legacy graph loads without embeddings", spec `embedding` § "Legacy graph loads without embeddings")
- `Scenario: Sidecar missing but pointer present` (spec `semantic-edge-inference` § "Sidecar missing but pointer present", spec `embedding` § "Sidecar missing but pointer present")

**PASS conditions**:
- `Graph` type has both `gEmbeddings :: Maybe (Map NodeId [Double])` and `gEmbeddingsPath :: Maybe FilePath` fields with `Nothing` defaults
- `ToJSON` on `Graph` omits `embeddings_path` when `Nothing`; writes `"embeddings_path": "embeddings.json"` when `Just "embeddings.json"`
- `FromJSON` on `Graph` defaults `embeddings_path` to `Nothing` when the field is absent; reads `Just path` when present
- Round-trip test: `decode (encode g) == Just g` for a `Graph` with `embeddings_path = Just "embeddings.json"` and `gEmbeddings = Just embs`
- Round-trip test: `decode (encode g) == Just g` for a `Graph` with both fields `Nothing`
- Legacy compatibility: decoding a `graph.json` without `embeddings_path` yields `gEmbeddingsPath = Nothing`
- Pipeline test: running with `--embed` produces both `graph.json` (with `embeddings_path` pointer) and `embeddings.json` (sidecar with `Map NodeId [Double]`)
- Pipeline test: running without `--embed` produces only `graph.json` (no `embeddings_path` pointer)
- Loader test: graph with sidecar loads `gEmbeddings = Just ...`
- Loader test: missing sidecar (pointer present, file absent) logs a warning and returns `gEmbeddings = Nothing`
- Loader test: legacy graph without pointer loads `gEmbeddings = Nothing`

**FAIL boundaries**:
- If `gEmbeddingsPath` is always written even when `Nothing` (non-omitting encoder), the test fails — the field must be omitted when `Nothing` to keep legacy graphs compatible
- If `FromJSON` fails when `embeddings_path` is absent instead of defaulting to `Nothing`, the test fails (legacy compatibility violation)
- If the pipeline writes `embeddings.json` even when `--embed` is not passed, the test fails
- If the loader throws an exception (not a warning) when the sidecar is missing, the test fails

### Affected modules

- **Modified**: `src/Graphos/Domain/Graph/Core.hs` — add fields, update `ToJSON`/`FromJSON`
- **Modified**: `src/Graphos/UseCase/Pipeline/Core.hs` — write `embeddings.json` sidecar
- **Modified**: `src/Graphos/UseCase/Load.hs` — follow pointer in `loadGraphFromFile`
- **New**: `test/Graphos/UseCase/EmbeddingsSpec.hs` — Hspec tests for sidecar load/store
- **Imports from**: `Data.Aeson` (serialization), `Data.Map.Strict` (Map operations), `System.IO` (file I/O)

### Prerequisites

- `Graph` type exists in `Domain/Graph/Core.hs` with existing `ToJSON`/`FromJSON` instances
- Pipeline has access to embeddings generated during the ingest step
- `loadGraphFromFile` already reads `graph.json` from disk
- `gNodes` and `gEdges` accessor functions are available

### Risks

- **Low**: Adding fields to `Graph` is additive — no breaking changes to existing code paths
- **Low**: `ToJSON`/`FromJSON` modifications use optional fields with defaults — backward compatible
- **Low**: File I/O for sidecar follows existing patterns in the codebase (e.g., `index.json` writing)
- **Medium**: Ensuring `embeddings.json` is written atomically before `graph.json` with the pointer — a race where `graph.json` is read before `embeddings.json` is complete could cause a missing-sidecar warning on valid data. Solution: write sidecar first, then graph with pointer (sequential, not atomic)
