## 1. Remove duplicate JSON write from exportAll

- [x] 1.P Plan: Remove the `ExportJSON.exportGraph` call from `exportAll` in `UseCase/Export.hs`. The `IncrementalJSON` writer in Pipeline already produces `graph.json`. Verify that `exportAll` no longer rewrites `graph.json`. Check criteria: (1) `cabal build` compiles without warnings, (2) `cabal test` passes, (3) after running graphos on a test codebase, `graph.json` exists and is complete (has nodes, edges, communities, cohesion, god_nodes keys), (4) no call to `ExportJSON.exportGraph` or `ExportJSON.exportGraphWithLabels` remains in `UseCase/Export.hs`.
- [x] 1.D Do: In `UseCase/Export.hs`, remove the line `ExportJSON.exportGraph g analysis jsonPath` and its import. If community labels need to be written, add a `writeAnalysisTail` call in `UseCase/Pipeline.hs` after clustering. Remove the `jsonPath` let-binding if it's only used by the removed call. Ensure `graph.json` is still written by the existing `IncrementalJSON` writer.
- [x] 1.C Check: Run `cabal build` (no warnings), `cabal test` (all pass). Run graphos on a small codebase. Verify `graph.json` contains all expected keys. Grep for `exportGraph` calls in `UseCase/Export.hs` — none should remain.
- [x] 1.A Act: If passed, standardize the pattern: IncrementalJSON is the sole writer for `graph.json`. Update any docs referencing the old export path.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Add explicit memory release boundaries in Pipeline

- [x] 2.P Plan: Add `evaluate` + `performGC` boundaries between pipeline phases in `UseCase/Pipeline.hs`. Specifically: (a) after `buildGraphFromExtractions`, evaluate the Graph and `performGC` to release Extraction Maps, (b) after `clusterGraphWithResolution` + `scoreAllCohesion`, the LeidenState is implicitly released (it's a local binding in the pure function), (c) after `analyzeGraph`, ensure CachedFGL can be reclaimed. Check criteria: (1) `cabal build` compiles, (2) `cabal test` passes, (3) running with `+RTS -s` shows reduced peak memory compared to baseline (measure on a test codebase with 1000+ nodes).
- [x] 2.D Do: In `UseCase/Pipeline.hs`, add `import System.Mem (performGC)` and `import Control.Exception (evaluate)`. After `buildGraphFromExtractions`, add: `evaluate (gNodes graph) >> evaluate (gEdges graph) >> performGC`. Replace the existing `graph `deepseq` pure ()` with the more targeted evaluation. After `analyzeGraph`, add `performGC` (the Analysis record is small; CachedFGL is local and reclaimable). Ensure `deepseq` is still used where needed (e.g., enriched graph after inferEdges).
- [x] 2.C Check: `cabal build` (no warnings), `cabal test` (all pass). Run graphos with `+RTS -s` on a test codebase. Verify peak memory is lower than before (capture baseline first). Verify node/edge counts are identical.
- [x] 2.A Act: If passed, document the memory boundary pattern in Pipeline.hs comments. If memory reduction is less than expected, profile with `+RTS -h` and identify remaining retention points.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Chunk extraction with GC between batches

- [ ] 3.P Plan: Modify `extractAll` in `UseCase/Extract.hs` to process files in bounded chunks of 500 with `performGC` between chunks. Add `evaluate` calls on each extraction result before accumulation to prevent thunk chains. Check criteria: (1) `cabal build` compiles, (2) `cabal test` passes, (3) extraction produces identical node/edge counts with chunk sizes 500 and 100, (4) `performGC` is called between chunks (verify by adding a trace or log message).
- [ ] 3.D Do: **BLOCKED** — Pre-existing build errors from `graphos-product` change (Extraction type changed from `[Node]` to `Map NodeId Node`) prevent compilation. The chunking changes require a working build. Must wait for the `graphos-product` change to be completed/merged, or apply changes on top of a clean build. Import additions (`System.Mem (performGC)` and `Control.Exception (evaluate)`) are ready. The chunking logic (wrap tree-sitter extraction in `chunkList 500` with `performGC` between chunks, add `evaluate` on extraction results before accumulating) is designed but cannot be applied until the type errors are resolved.
- [ ] 3.C Check: `cabal build` (no warnings), `cabal test` (all pass). Run graphos with chunk size 500 and chunk size 100 — verify identical output. Run with `+RTS -s` and verify reduced peak memory during extraction phase.
- [ ] 3.A Act: If passed, standardize chunk size as a configurable parameter (add to `PipelineConfig` or `ExtractorConfig` in a future change).

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Stream HTML export to file Handle

- [ ] 4.P Plan: Rewrite `exportHTML` in `Infrastructure/Export/HTML.hs` to stream nodes and edges to a file `Handle` one-by-one instead of building the entire HTML as a `Text` in memory. Use `Data.Aeson.encode` on individual `Node`/`Edge` values and write to the Handle. Check criteria: (1) `cabal build` compiles, (2) `cabal test` passes, (3) running graphos produces a `graph.html` that renders correctly in a browser, (4) the node/edge data in the HTML is identical to the previous implementation's output (compare JSON arrays).
- [ ] 4.D Do: Create a streaming HTML writer that opens a `Handle`, writes the HTML header and vis.js setup, then streams nodes as individual JSON objects with comma separators, then streams edges similarly, then writes the footer. Replace `buildHTML` (which constructs the full `Text`) with `streamHTML` (which writes to `Handle`). Use `hPutStr` and `BSL.hPut` for writing. Keep the vis.js template and styling identical.
- [ ] 4.C Check: `cabal build` (no warnings), `cabal test` (all pass). Run graphos on a test codebase. Open `graph.html` in a browser — verify it renders correctly. Compare the embedded JSON data to the previous implementation's output (same nodes, same edges, same communities). Run on a large codebase and verify no OOM during HTML export.
- [ ] 4.A Act: If passed, update HTML export to always use streaming. If rendering issues arise, fall back to batch approach for HTML only and keep other streaming changes.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Stream Cypher export to file Handle

- [ ] 5.P Plan: Modify `exportCypher` in `Infrastructure/Export/Neo4j.hs` and `exportMemgraphCypher` in `Infrastructure/Export/Memgraph.hs` to write Cypher statements to a file `Handle` incrementally instead of building the full `Text` in memory. Check criteria: (1) `cabal build` compiles, (2) `cabal test` passes, (3) generated Cypher files contain valid CREATE statements, (4) statement content is identical to previous implementation (order-independent comparison for node statements).
- [ ] 5.D Do: Create `streamCypherToFile :: Graph -> FilePath -> IO ()` that opens a `Handle`, writes `CREATE` statements for each node one-by-one, then `CREATE` statements for edges, then `CREATE` statements for community nodes and BELONGS_TO edges (if communities are provided). Replace `generateCypher` (which builds full `Text`) with `streamCypherToFile`. Keep `generateCypher` available for the push-to-Neo4j path (which needs statements in batches). Add `import System.IO (Handle, IOMode(..), hPutStr, hFlush, hClose, openFile)`.
- [ ] 5.C Check: `cabal build` (no warnings), `cabal test` (all pass). Run graphos on a test codebase. Compare the `.cypher` file to previous output — same CREATE statements (order-independent). Run on a large codebase and verify no OOM during Cypher export.
- [ ] 5.A Act: If passed, standardize the streaming pattern. Consider extracting a `StreamingWriter` utility module for reuse across HTML, Cypher, and other export formats.

### Attempt history (5)

<!-- empty unless a retry is needed -->

## 6. Integration test: full pipeline memory profile

- [ ] 6.P Plan: Run the full pipeline on a large test codebase (or synthetic 50k-node graph) with `+RTS -s` and verify that peak memory is ≤3× the final graph size. Capture before/after metrics. Check criteria: (1) `cabal test` passes, (2) `graph.json`, `graph.html`, and `.cypher` files are produced and valid, (3) peak memory measured by `+RTS -s` is ≤3× the final graph size (estimated from total bytes in heap), (4) no OOM crash.
- [ ] 6.D Do: Create a test script that runs `graphos` on a real codebase with `+RTS -s -h` and captures memory statistics. Compare peak memory before and after all changes. Verify all output files are present and valid. Run `cabal test` to ensure no regressions.
- [ ] 6.C Check: All output files produced. Peak memory is reduced compared to baseline. No test failures. No OOM on previously-crashing codebases.
- [ ] 6.A Act: If passed, document the memory profile results in the change summary. If peak memory exceeds 3× target, profile with `+RTS -h` and identify remaining retention points for a follow-up PDCA cycle.

### Attempt history (6)

<!-- empty unless a retry is needed -->