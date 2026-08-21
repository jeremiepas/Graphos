## 1. Add RTS profiling and heap limit CLI flags

- [ ] 1.P Plan: Add `--rts-profile` and `--max-heap SIZE` flags to `app/Main.hs`. Check criteria: (1) `graphos . --rts-profile` produces GC stats on stderr, (2) `graphos . --max-heap 1G` fails with clear error when heap exceeds 1GB, (3) `cabal test` passes, (4) both flags can be combined. Affected: `app/Main.hs`. Risk: RTS options must be set before GHC runtime initializes — may need `+RTS` in executable wrapper or `setRTSOpts` from `GHC.RTS.Flags`.
- [ ] 1.D Do: Implement CLI flag parsing in `Main.hs`. Use `GHC.RTSFlags.setRTSOpts` or modify the cabal `ghc-options` to support dynamic RTS flags. Add `--rts-profile` (enables `+RTS -s -h`) and `--max-heap` (enables `+RTS -M <size>`). Write error message for heap exhaustion.
- [ ] 1.C Check: (1) Run `graphos . --rts-profile` on a small test directory — verify GC stats appear on stderr and `.hp` file is generated. (2) Run `graphos . --max-heap 1G` on a codebase that exceeds 1GB — verify graceful failure. (3) Run `cabal test`. (4) Run `graphos . --rts-profile --max-heap 4G` — verify both work together.
- [ ] 1.A Act: If all checks pass, mark done. If `setRTSOpts` doesn't work (GHC limitation), fall back to documenting `+RTS` syntax in `--help` output.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Replace diff-list edge accumulators with Map

- [ ] 2.P Plan: In `UseCase.Extract`, replace all `IORef ([Edge] -> [Edge])` accumulators with `IORef (Map EdgeId Edge)`. Replace diff-list append `acc . (edges ++)` with `Map.union`. Check criteria: (1) No `[Edge] -> [Edge]` type remains in `UseCase.Extract`, (2) `cabal test` passes, (3) extraction produces identical node/edge counts on a test codebase. Affected: `src/Graphos/UseCase/Extract.hs`. Risk: `Map.union` may be slightly slower for small batches but faster for large merges.
- [ ] 2.D Do: Change `codeEdgeAccRef`, `docEdgeAccRef`, `officeEdgeAccRef`, `imageEdgeAccRef` from `IORef ([Edge] -> [Edge])` to `IORef (Map EdgeId Edge)`. Update `accumulateEdges` to use `Map.union`. Update final merge to read `Map EdgeId Edge` directly. Ensure `EdgeId` is properly derived for all edges.
- [ ] 2.C Check: (1) `grep -r "Edge -> \[Edge\]" src/Graphos/UseCase/Extract.hs` returns nothing. (2) `cabal test` passes. (3) Run `graphos .` on a test codebase — node and edge counts match pre-change output.
- [ ] 2.A Act: If all checks pass, mark done. If edge deduplication changes counts, verify that `Map.union` right-bias is correct (newer edges should win on conflict).

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Implement batch extraction merge with incremental GC

- [ ] 3.P Plan: Restructure `extractAll` to merge extraction results batch-by-batch, calling `evaluate` + `performGC` after each batch. Check criteria: (1) Peak memory during extraction is bounded (no monotonic growth), (2) `cabal test` passes, (3) total node/edge counts match pre-change. Affected: `src/Graphos/UseCase/Extract.hs`. Risk: GC pauses between batches may slow extraction slightly.
- [ ] 3.D Do: Refactor `extractAll` to process categories (code, doc, office, image) in sequence or bounded parallelism. After each category's extraction completes, merge into the running `Extraction`, evaluate `Map.size`, call `performGC`. Remove the 8 separate IORefs and use a single `IORef Extraction` or direct return values.
- [ ] 3.C Check: (1) Run `graphos . +RTS -s` on a 5k+ file codebase before and after — verify peak memory is lower. (2) `cabal test` passes. (3) Total node/edge counts in output match.
- [ ] 3.A Act: If GC pauses cause noticeable slowdown (>20% increase in extraction time), reduce GC frequency (every N batches instead of every batch). If results match, mark done.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Add bounded LSP concurrency

- [ ] 4.P Plan: Cap concurrent LSP server processes at a configurable limit (default 2). Replace `mapConcurrently` for LSP groups with a bounded pool. Check criteria: (1) At most N LSP processes alive at any time (N = `--lsp-concurrency`), (2) `disconnectLSP` is called after each group, (3) `cabal test` passes, (4) extraction produces identical results. Affected: `src/Graphos/UseCase/Extract.hs`, `app/Main.hs` (new CLI flag). Risk: Sequential LSP extraction may be slower for single-language codebases but faster for multi-language (less GC pressure).
- [ ] 4.D Do: Add `--lsp-concurrency` flag to `PipelineConfig`. Replace `mapConcurrently (extractGroup ...)` with a bounded pool using `QSemN` or `withPool`. Ensure `disconnectLSP` is called in a `bracket` after each group completes. Update `extractAll` to respect the concurrency limit.
- [ ] 4.C Check: (1) Run `graphos .` on a multi-language codebase (3+ languages) — verify at most 2 LSP processes alive at any time via `ps aux | grep language-server`. (2) Verify `disconnectLSP` is called for each group. (3) `cabal test` passes. (4) Total node/edge counts match pre-change.
- [ ] 4.A Act: If extraction throughput regresses >20% on single-language codebases, increase default concurrency to 3 or make it adaptive. Otherwise mark done.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Add in-place graph edge enrichment

- [ ] 5.P Plan: Add `addEdges :: Graph -> [Edge] -> Graph` to `Domain.Graph.Core`. Replace `buildGraphFromExtractions` call in pipeline edge inference with `addEdges`. Check criteria: (1) `addEdges` inserts edges into existing Maps without creating intermediate `Extraction`, (2) `cabal test` passes, (3) enriched graph has same nodes + inferred edges as before. Affected: `src/Graphos/Domain/Graph/Core.hs`, `src/Graphos/UseCase/Pipeline.hs`. Risk: `addEdges` must correctly update `gAdjFwd` and `gAdjBack`.
- [ ] 5.D Do: Implement `addEdges` in `Domain.Graph.Core` — for each edge, insert into `gEdges` and update both `gAdjFwd` and `gAdjBack`. Add Hspec tests for `addEdges`. Update `UseCase.Pipeline` to use `addEdges graph inferredEdges` instead of `buildGraphFromExtractions`. Remove the `extractionFromLists` call that copies all nodes+edges.
- [ ] 5.C Check: (1) `addEdges` unit tests pass (empty list, single edge, multiple edges, duplicate edges, dangling edges). (2) `cabal test` passes. (3) Run full pipeline — enriched graph has same node count as original, edge count = original + inferred.
- [ ] 5.A Act: If `addEdges` has edge cases (dangling edges, directed vs undirected), add validation. Otherwise mark done.

### Attempt history (5)

<!-- empty unless a retry is needed -->

## 6. Bound observability stores

- [ ] 6.P Plan: Add capacity limits to `tracerSpans` (1000), `msHistograms` (pre-aggregate), and `dtBuffer` (10000 with disk flush). Check criteria: (1) Spans are bounded to last N, (2) Histograms use O(1) memory per metric, (3) Debug trace flushes to disk at capacity, (4) `cabal test` passes. Affected: `src/Graphos/Infrastructure/Observability.SDK.hs` only (dead `Observability.hs` was removed by `cleanup-ram-fix-prework`). Risk: Changing histogram type may affect Prometheus rendering.
- [ ] 6.D Do: (a) Replace `IORef [Span]` with a bounded buffer type that evicts oldest when full. (b) Replace `IORef (Map HistogramName [Double])` with `IORef (Map HistogramName HistogramAgg)` where `HistogramAgg` = {count, sum, min, max, buckets}. (c) Update `renderPrometheusMetrics` to render from `HistogramAgg`. (d) Add disk flush to `dtBuffer` when it reaches capacity.
- [ ] 6.C Check: (1) Insert 10k spans — verify only last 1000 are retained. (2) Insert 100k histogram observations — verify memory is O(1) per metric. (3) Insert 20k debug trace events — verify JSONL file has all 20k, memory has at most 10k. (4) `cabal test` passes. (5) Prometheus rendering still produces valid output.
- [ ] 6.A Act: If Prometheus rendering breaks, fix `HistogramAgg` rendering to match expected format. If disk flush has I/O errors, add error handling. Otherwise mark done.

### Attempt history (6)

<!-- empty unless a retry is needed -->

## 7. Compact Node representation

- [ ] 7.P Plan: Replace remaining `Maybe` fields in `Node` with a packed representation using a bit-field for presence flags and `Data.Text.Short` for `nodeLabel`, `nodeSourceFile`, `nodeSignature` (and any other short `Text` fields). Keep JSON output identical. Check criteria: (1) JSON round-trip identity (decode→encode produces same JSON), (2) Per-node memory reduced by ~30-40% beyond the win from removing legacy fields, (3) `cabal test` passes including all Node-related tests. Affected: `src/Graphos/Domain/Types/Node.hs`, `src/Graphos/Domain/Types.hs` (re-exports). Risk: `text-short` is a new dependency. JSON serialization must remain identical.
- [ ] 7.D Do: Add `text-short` to `graphos.cabal` build-depends. Add a `Word64 nodePresentBits` field to `Node` to track presence of optional fields; keep `nodeExtra :: Maybe Value` unchanged so `nodeExtraCapturedAt`/`setNodeExtraCapturedAt` helpers remain valid. Use `Data.Text.Short` for `nodeLabel`, `nodeSourceFile`, and `nodeSignature`. Update `ToJSON`/`FromJSON` instances to produce/consume identical JSON. Update all pattern matches on `Node` fields throughout the codebase. Add Hspec round-trip test.
- [ ] 7.C Check: (1) JSON round-trip: `fromJSON (toJSON node) == node` for representative nodes. (2) Heap profile: 100k nodes occupy <20MB in `Map NodeId Node`. (3) `cabal test` — all existing tests pass. (4) Full pipeline run produces identical `graph.json` (structural comparison).
- [ ] 7.A Act: If any test breaks due to pattern matching changes, fix case-by-case. If `Text.Short` causes issues with very long labels, ensure it handles them correctly (it should, as `Text.Short` handles arbitrary lengths). Otherwise mark done.

### Attempt history (7)

<!-- empty unless a retry is needed -->

## 8. Integration test and memory profiling

- [ ] 8.P Plan: Run the full pipeline on a 50k+ file multi-language codebase with `--rts-profile` and verify peak memory <8GB. Check criteria: (1) `+RTS -s` shows peak heap <8GB, (2) No OOM crash, (3) All 7 previous tasks pass their own checks. Affected: integration testing only. Risk: Test codebase may not be available or may be too small.
- [ ] 8.D Do: Create or find a test codebase with 50k+ files spanning at least 3 languages. Run `graphos . --rts-profile --max-heap 8G`. Collect GC statistics. Compare output `graph.json` with pre-change baseline.
- [ ] 8.C Check: (1) Peak heap from `+RTS -s` output <8GB. (2) No OOM crash during full pipeline. (3) Output `graph.json` is structurally similar to baseline (within 1% tolerance for community detection non-determinism). (4) All `cabal test` pass.
- [ ] 8.A Act: If peak memory exceeds 8GB, identify which phase causes the spike and optimize further. If all checks pass, update `.opencode/context/core/standards/code-quality.md` with memory-aware patterns (bounded buffers, incremental merge, compact types). Mark change as verified.

### Attempt history (8)

<!-- empty unless a retry is needed -->