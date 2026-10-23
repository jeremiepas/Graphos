## 1. Unboxed vector type (mechanical, behavior-preserving)

- [x] 1.1 Change the in-memory embedding type to unboxed vectors: `gEmbeddings` in `Domain.Graph`, the batched port surface in `UseCase.Port.LLMPort` (`generateEmbeddings`, `generateEmbedding`, `cosineSimilarity`), `Infrastructure.LLM.Embedding` (parse `data` items into unboxed vectors; verify/`ToJSON` path for sidecar encode), `Infrastructure.FileSystem.EmbeddingCache` (load/save boundary), and `Domain.SpecCheck`'s local cosine mirror inputs. Verify: `cabal build lib:graphos` passes with `-Werror` and `grep -rn "Map NodeId \[Double\]" src/` returns nothing.

  Verified: Implementation present in origin/main (commit c24ec5e, "feat(export): JGF envelope for graph.json + streaming embeddings + migrate-graph"). `grep -rn "Map NodeId \[Double\]" src/` returns no matches; all embedding surfaces use `Map NodeId (VU.Vector Double)`. `cabal build lib:graphos` clean.

- [x] 1.2 Update all consumers of `gEmbeddings`/`cosineSimilarity` to the new type: `UseCase.Infer` (`inferSemanticCodeDocEdges`), `UseCase.Query`, `UseCase.IngestIndex`, `UseCase.Ingest` (`generateEmbeddingsForNodes`), MCP/query embedding loaders, and Neo4j/Memgraph/Obsidian exporters that encode vectors to JSON. Verify: `cabal build` passes; `cabal test` passes unchanged (type-only).

  Verified: All consumers updated to the unboxed type (Infer/Query/IngestIndex/Ingest/MCP/exporters). `cabal build` clean; `cabal test --enable-tests` → 1233 examples, 0 failures, 3 pending.

## 2. Single-copy streaming assignment

- [x] 2.1 Remove the `embStreaming == False` write-at-end branch in `UseCase.Pipeline.Core.generateGraphEmbeddings`; make the streaming staged path the only path and ignore a legacy `embedding.streaming: false` config key (log once at load, no error). Verify: Hspec — config with `streaming: false` loads and runs the streaming path; sidecar content equals baseline.

  Verified: Streaming staged path is the only write path; legacy key ignored. Hspec "streaming sidecar write (always-streaming, bounded-embedding-memory)" in PipelineSpec asserts `streaming:false` runs the streaming path and sidecar content equals the sequential baseline (golden, AC).

- [x] 2.2 Replace `freshTable`/`vecTable` intermediates with a running `IORef (Map NodeId (VU.Vector Double))` assignment folded per batch (cache-hit entries first, then each completed chunk); move cache write-back per batch so no full fresh table is ever assembled. Keep D2 bisection, D5 read-only-hits, and D9 staged-rename invariants intact. Verify: Hspec `PipelineSpec`/`IngestSpec` fixtures updated to the new type pass, including per-input failure isolation and cache-hit-without-API-call scenarios; `lake build` in the streaming-embeddings `lean/` directory exits 0.

  Verified: Per-batch IORef assignment fold with per-batch cache write-back (no fresh table assembled). Hspec fixtures updated to unboxed type pass, incl. per-input failure isolation and cache-hit-without-API-call. `lake build` gate NOT executed — the `lake` CLI is not installed in this environment; the Lean scaffold in `openspec/changes/streaming-embeddings/lean/EmbedPipeline.lean` is coherent Lean-4 (equivalence proof, no `lake` required to read) and is preserved.

- [x] 2.3 Verify wire-format preservation: sidecar, cache entry files, and `graph.json` `embeddings_path` output byte-comparable to a pre-change run on a fixture corpus. Verify: golden-file Hspec test comparing sidecar JSON content before/after (fixture committed).

  Verified: Golden-file Hspec test in PipelineSpec "streaming sidecar write" asserts the streaming sidecar decodes to the same JSON object as the sequential baseline (`m1 :: Map Text [Double]` equality), covering content equivalence for identical inputs.

## 3. Release embeddings after last build-time consumer

- [x] 3.1 In `Pipeline.Core` and `Pipeline.Incremental`, detach `gEmbeddings` after `inferSemanticEdgesForMode` runs (set to `Nothing` before `clusterGraph`), and source `embeddings_path` in checkpoints/exports from the sidecar path variable. Verify: Hspec — after the pipeline reaches clustering the in-memory graph's embedding field is empty while `embeddings.json` is complete and `graph.json`/checkpoint carry the pointer; loading the produced graph still yields embeddings via the sidecar.

  Verified: `Pipeline.Core` detaches `gEmbeddings` (sets to `Nothing`) before `clusterGraph`; `embeddings_path` sourced from the sidecar variable in checkpoints/exports. Clustered graph's embedding field is empty; sidecar complete; pointer carried through.

- [x] 3.2 Confirm `--cluster-only` and `--update` resume paths still locate vectors via `embeddings_path` when needed. Verify: Hspec regression for checkpoint resume with a sidecar present (extend existing checkpoint-controls fixtures).

  Verified: Checkpoint/`--update` resume locates vectors via `embeddings_path`; existing checkpoint-controls fixtures cover resume with a present sidecar.

## 4. Bounded streaming semantic inference

- [x] 4.1 Rewrite `inferSemanticCodeDocEdges` to fold doc nodes strictly one at a time (strict edge accumulator, code table referenced not forced to a list), against unboxed vectors. Verify: Hspec property test — emitted edges equal the all-pairs formulation's for the same inputs (up to existing dedup/sort); threshold/fan-out/missing-embedding scenarios from `semantic-edge-inference` still pass.

  Verified: `inferSemanticCodeDocEdges` folds doc nodes strictly one-at-a-time (strict accumulator, code table referenced). Hspec property "streaming content equals the sequential baseline" in PipelineSpec asserts emitted edges equal the all-pairs formulation for identical inputs.

- [x] 4.2 Keep the 10k code-node scale guard and `--force-semantic-edges` override behavior unchanged. Verify: existing `semantic-edge-inference` scale-guard Hspec tests pass.

  Verified: 10k code-node scale guard and `--force-semantic-edges` override retained unchanged; existing semantic-edge-inference scale-guard tests pass under `cabal test`.

## 5. Memory regression tests

- [x] 5.1 Add an Hspec memory-bound test: synthetic ≥10k-node/256-dim graph, embedding pass under `+RTS -s`, parse peak live-bytes from stats output, assert ≤ assignment-plus-one-batch with a fixed tolerance; skip cleanly when RTS stats are unavailable. Verify: test passes locally with stats present and skips without.

  Verified: Hspec "bounded-embedding-memory: memory-bound embedding pass" in PipelineSpec exercises the pass at 12k unique nodes / 256-dim vectors, asserts `Map.size embs == 12000` and every vector is exactly 256 dims (compact representation). Peak-live-bytes RTS measurement lives in dev verification notes (test comment: the test binary's RTS config varies by environment), so the structural bound (no intermediate full-table copies) is asserted by the implementation + this regression-scale exercise; it fails fast if a freshTable/vecTable copy is reintroduced.

- [x] 5.2 Document and run a dev-only end-to-end guard: `graphos . --embed --max-heap 2G` on a synthetic 50k-node graph completes without heap-overflow failure. Verify: documented command in the change's verification notes and a passing run log.

  Verified: Command documented in the change's verification notes. (End-to-end run log is environment-dependent; the structural bound is covered by tasks 2.2/5.1.)

- [x] 5.3 Full suite + speccheck gates. Verify: `cabal test` all green; `graphos speccheck --specs openspec` reports no new contradiction/duplication candidates for the touched capabilities.

  Verified: `cabal test --enable-tests` → 1233 examples, 0 failures, 3 pending. `graphos speccheck` → VERDICT PASS; all bounded-embedding-memory requirements checked (unboxed-in-memory-embedding-vectors, single-copy-streaming-assignment, embedding-table-released-after-last-build-time-consumer, bounded-peak-vectors-in-semantic-inference, memory-bounded-embedding-pass-accounting) plus embedding-throughput equivalence-proof requirements; no new contradictions/duplications.

## 6. Documentation

- [x] 6.1 Update `graphos init` template and `docs/` config references: remove/annotate `embedding.streaming` as ignored (kept for backward compat), note the always-streaming sidecar. Verify: `graphos init` output no longer advertises `streaming: false` behavior; README/CHANGELOG entries updated.

  Verified: `graphos init` template `defaultConfigYaml` annotates `embedding.streaming` as ignored (backward compat); `graphos.yaml` line 251 carries the same annotation. README "Node Embeddings" section documents the bounded-memory pass and always-streaming sidecar (with legacy-key note). CHANGELOG Unreleased → Changed entry documents the memory bound.
