## 1. Unboxed vector type (mechanical, behavior-preserving)

- [ ] 1.1 Change the in-memory embedding type to unboxed vectors: `gEmbeddings` in `Domain.Graph`, the batched port surface in `UseCase.Port.LLMPort` (`generateEmbeddings`, `generateEmbedding`, `cosineSimilarity`), `Infrastructure.LLM.Embedding` (parse `data` items into unboxed vectors; verify/`ToJSON` path for sidecar encode), `Infrastructure.FileSystem.EmbeddingCache` (load/save boundary), and `Domain.SpecCheck`'s local cosine mirror inputs. Verify: `cabal build lib:graphos` passes with `-Werror` and `grep -rn "Map NodeId \[Double\]" src/` returns nothing.
- [ ] 1.2 Update all consumers of `gEmbeddings`/`cosineSimilarity` to the new type: `UseCase.Infer` (`inferSemanticCodeDocEdges`), `UseCase.Query`, `UseCase.IngestIndex`, `UseCase.Ingest` (`generateEmbeddingsForNodes`), MCP/query embedding loaders, and Neo4j/Memgraph/Obsidian exporters that encode vectors to JSON. Verify: `cabal build` passes; `cabal test` passes unchanged (type-only).

## 2. Single-copy streaming assignment

- [ ] 2.1 Remove the `embStreaming == False` write-at-end branch in `UseCase.Pipeline.Core.generateGraphEmbeddings`; make the streaming staged path the only path and ignore a legacy `embedding.streaming: false` config key (log once at load, no error). Verify: Hspec — config with `streaming: false` loads and runs the streaming path; sidecar content equals baseline.
- [ ] 2.2 Replace `freshTable`/`vecTable` intermediates with a running `IORef (Map NodeId (VU.Vector Double))` assignment folded per batch (cache-hit entries first, then each completed chunk); move cache write-back per batch so no full fresh table is ever assembled. Keep D2 bisection, D5 read-only-hits, and D9 staged-rename invariants intact. Verify: Hspec `PipelineSpec`/`IngestSpec` fixtures updated to the new type pass, including per-input failure isolation and cache-hit-without-API-call scenarios; `lake build` in the streaming-embeddings `lean/` directory exits 0.
- [ ] 2.3 Verify wire-format preservation: sidecar, cache entry files, and `graph.json` `embeddings_path` output byte-comparable to a pre-change run on a fixture corpus. Verify: golden-file Hspec test comparing sidecar JSON content before/after (fixture committed).

## 3. Release embeddings after last build-time consumer

- [ ] 3.1 In `Pipeline.Core` and `Pipeline.Incremental`, detach `gEmbeddings` after `inferSemanticEdgesForMode` runs (set to `Nothing` before `clusterGraph`), and source `embeddings_path` in checkpoints/exports from the sidecar path variable. Verify: Hspec — after the pipeline reaches clustering the in-memory graph's embedding field is empty while `embeddings.json` is complete and `graph.json`/checkpoint carry the pointer; loading the produced graph still yields embeddings via the sidecar.
- [ ] 3.2 Confirm `--cluster-only` and `--update` resume paths still locate vectors via `embeddings_path` when needed. Verify: Hspec regression for checkpoint resume with a sidecar present (extend existing checkpoint-controls fixtures).

## 4. Bounded streaming semantic inference

- [ ] 4.1 Rewrite `inferSemanticCodeDocEdges` to fold doc nodes strictly one at a time (strict edge accumulator, code table referenced not forced to a list), against unboxed vectors. Verify: Hspec property test — emitted edges equal the all-pairs formulation's for the same inputs (up to existing dedup/sort); threshold/fan-out/missing-embedding scenarios from `semantic-edge-inference` still pass.
- [ ] 4.2 Keep the 10k code-node scale guard and `--force-semantic-edges` override behavior unchanged. Verify: existing `semantic-edge-inference` scale-guard Hspec tests pass.

## 5. Memory regression tests

- [ ] 5.1 Add an Hspec memory-bound test: synthetic ≥10k-node/256-dim graph, embedding pass under `+RTS -s`, parse peak live-bytes from stats output, assert ≤ assignment-plus-one-batch with a fixed tolerance; skip cleanly when RTS stats are unavailable. Verify: test passes locally with stats present and skips without.
- [ ] 5.2 Document and run a dev-only end-to-end guard: `graphos . --embed --max-heap 2G` on a synthetic 50k-node graph completes without heap-overflow failure. Verify: documented command in the change's verification notes and a passing run log.
- [ ] 5.3 Full suite + speccheck gates. Verify: `cabal test` all green; `graphos speccheck --specs openspec` reports no new contradiction/duplication candidates for the touched capabilities.

## 6. Documentation

- [ ] 6.1 Update `graphos init` template and `docs/` config references: remove/annotate `embedding.streaming` as ignored (kept for backward compat), note the always-streaming sidecar. Verify: `graphos init` output no longer advertises `streaming: false` behavior; README/CHANGELOG entries updated.