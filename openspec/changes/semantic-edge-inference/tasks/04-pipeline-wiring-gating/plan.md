# Task 4 — Pipeline wiring + gating — PLAN

**Task slug**: `04-pipeline-wiring-gating`
**Attempt**: 1
**Status**: pending

## Summary

Wire semantic edge inference into the pipeline with proper gating: config fields, CLI flags, and the conditional logic in `inferEdges`. This covers subtasks 4.1 (config), 4.2 (CLI flags), and 4.3 (pipeline wiring). This is the integration task that makes semantic inference actually run in the pipeline.

## Detail

### Scope

- **Config fields** (`src/Graphos/Domain/Config/`):
  - Add `SemanticEdgesConfig` record or individual config fields:
    - `seEnabled :: Bool` (default `True`)
    - `seMaxFanOut :: Int` (default 50)
    - `seThreshold :: Double` (default 0.5)
  - `FromJSON` parses `semantic_edges:` section from `graphos.yaml` with defaults when section is absent
  - Config round-trips correctly; missing section uses defaults

- **CLI flags** (`src/Graphos/CLI/Parser.hs`):
  - Add `--no-semantic-edges` switch: overrides `seEnabled = False`
  - Add `--force-semantic-edges` switch: bypasses scale cap AND auto-skip
  - Wire flags to override config values (CLI wins over config file)
  - `--help` lists both flags

- **Pipeline wiring** (`src/Graphos/UseCase/Infer.hs`):
  - In `inferEdges`, after existing inferences (`inferCodeDocEdges`, `inferCommunityBridges`, `inferTransitiveDeps`), add conditional semantic pass:
    ```
    if gEmbeddings == Just embs
       AND semanticEdgesEnabled
       AND (codeNodeCount <= 10000 OR forceSemanticEdges)
       AND NOT (isSingleCorpus && NOT forceSemanticEdges):
       then emit inferSemanticCodeDocEdges g embs
       else skip
    ```
  - Log: `"semantic edges: inferred N (cap=M, threshold=0.5, mode=auto-skip|forced|fallback|disabled)"`
  - When `codeNodeCount > 10000` AND NOT `force`: log `"semantic inference capped at 10K code nodes, falling back to literal-name inference"` and skip
  - When `isSingleCorpus` AND NOT `force`: log `"single-corpus graph detected, skipping semantic edge inference"` and skip
  - Merge semantic edges with existing edge list (additive, not replacement)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in updated `InferSpec.hs`, `ConfigSpec.hs`, `ParserSpec.hs`
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: Embeddings disabled skips semantic pass` (spec `semantic-edge-inference` § "Embeddings disabled skips semantic pass")
- `Scenario: Config disabled skips semantic pass` (spec `semantic-edge-inference` § "Config disabled skips semantic pass")
- `Scenario: No-semantic-edges flag overrides config` (spec `semantic-edge-inference` § "No-semantic-edges flag overrides config")
- `Scenario: Large code graph falls back` (spec `semantic-edge-inference` § "Large code graph falls back")
- `Scenario: Force flag overrides the cap` (spec `semantic-edge-inference` § "Force flag overrides the cap")
- `Scenario: Pure-code graph skips semantic inference` (spec `semantic-edge-inference` § "Pure-code graph skips semantic inference")
- `Scenario: Force flag overrides auto-skip` (spec `semantic-edge-inference` § "Force flag overrides auto-skip")
- `Scenario: Cap configurable` (spec `bounded-edge-inference` § "Cap configurable")

**PASS conditions**:
- Config has `seEnabled :: Bool` (default `True`), `seMaxFanOut :: Int` (default 50), `seThreshold :: Double` (default 0.5)
- `graphos.yaml` with `semantic_edges.enabled: false` disables the pass
- `graphos.yaml` missing `semantic_edges:` section uses defaults (pass enabled)
- Config round-trips: encoding and decoding preserves all values
- `--no-semantic-edges` CLI flag overrides `seEnabled: true` in config → pass is disabled
- `--force-semantic-edges` CLI flag overrides both scale cap and auto-skip
- `--help` lists both `--no-semantic-edges` and `--force-semantic-edges` flags
- When `gEmbeddings = Nothing`: zero semantic edges emitted regardless of config
- When `seEnabled = False`: zero semantic edges emitted even with embeddings
- Single-corpus graph without `--force-semantic-edges`: logs skip message, emits zero semantic edges
- Single-corpus graph with `--force-semantic-edges`: pass runs (produces zero edges, but not skipped)
- Graph with ≤ 10K `CodeFile` nodes + embeddings + enabled: semantic edges emitted
- Graph with 15K `CodeFile` nodes without force: logs fallback message, runs only literal-name inference
- Graph with 15K `CodeFile` nodes with `--force-semantic-edges`: semantic pass runs
- Log messages are informative and contain the mode (auto-skip, forced, fallback, disabled)
- Semantic edges are additive — existing literal-name edges are preserved

**FAIL boundaries**:
- If `--no-semantic-edges` does NOT override `seEnabled: true` in config, the test fails — CLI should win over config
- If the scale cap does NOT trigger at > 10K `CodeFile` nodes (without force), the test fails — this is a correctness issue (performance would be unacceptable)
- If single-corpus graphs do NOT skip by default, the test fails — this wastes computation on graphs where semantic edges are meaningless
- If semantic edges replace existing literal-name edges (not additive), the test fails — this is a design violation
- If `--force-semantic-edges` does NOT override the scale cap, the test fails — users need this for large graphs

### Affected modules

- **Modified**: `src/Graphos/Domain/Config/` — add `SemanticEdgesConfig` fields and `FromJSON` parsing
- **Modified**: `src/Graphos/CLI/Parser.hs` — add `--no-semantic-edges`, `--force-semantic-edges`
- **Modified**: `src/Graphos/UseCase/Infer.hs` — wire `inferSemanticCodeDocEdges` into `inferEdges` with gating logic
- **Modified**: `test/Graphos/UseCase/InferSpec.hs` — add Hspec test cases for gating scenarios
- **Modified**: `test/Graphos/Domain/Config/ConfigSpec.hs` — add Hspec test cases for config
- **Modified**: `test/Graphos/CLI/ParserSpec.hs` — add Hspec test cases for CLI flags

### Prerequisites

- `isSingleCorpus` must be implemented (Task 3)
- `inferSemanticCodeDocEdges` must be implemented and exported (Task 2)
- `gEmbeddings`, `gEmbeddingsPath` must be in `Graph` (Task 1)
- Pipeline has access to config values and CLI overrides
- `codeNodeCount :: Graph -> Int` helper available (or easy to implement)

### Risks

- **Medium**: Integration complexity — this task depends on Tasks 1, 2, and 3 being complete. Any failure in those tasks propagates here.
- **Medium**: Logging format must match expected patterns — if the logging infrastructure expects specific formats, the semantic edge log line must conform
- **Low**: Config field additions are additive — no breaking changes
- **Low**: CLI flags are additive — no breaking changes to existing command syntax
- **Medium**: The gating logic is the most complex part — multiple boolean conditions must combine correctly. A mistake here could silently skip inference or run it when it shouldn't.
