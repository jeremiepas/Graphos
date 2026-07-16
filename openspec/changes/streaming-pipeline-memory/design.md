## Context

Graphos processes codebases through a pipeline: detect → extract → build → cluster → infer → analyze → export. On large codebases (10k+ files, 100k+ nodes), the pipeline holds all intermediate data structures in memory simultaneously. The current memory lifecycle is:

1. **Extraction**: `IORef` accumulators hold all nodes/edges. LSP file contents kept in memory per file. `mapConcurrently` processes all files per group without memory bounds.
2. **Build**: `buildGraphFromExtractions` creates a `Graph` from the extraction Maps. Both the Extraction Maps and the Graph coexist because Haskell's GC cannot prove the Extraction is dead.
3. **Cluster**: `LeidenState` (IntMap + Unboxed Vectors) is built from the Graph. The Graph remains live because analysis needs it later.
4. **Analyze**: `CachedFGL` (Patricia Tree) is built from the Graph. On 100k-node graphs, this adds ~200MB.
5. **Export**: `exportAll` builds entire HTML strings, Cypher strings, and JSON in memory. Additionally, `ExportJSON.exportGraph` rewrites `graph.json` that `IncrementalJSON` already produced.

The codebase already has partial solutions: `StrictData` pragmas, `deepseq` calls, `IncrementalJSON` writer, `IORef` accumulators with DList pattern. But these are patches on a fundamentally batch-oriented architecture.

Peak memory on large codebases: 6-10× final graph size. On a 4GB RAM machine, this means OOM crash at ~50k nodes.

## Goals / Non-Goals

**Goals:**
- Reduce peak memory from 6-10× to 2-3× final graph size
- Enable 100k-node graphs to process on 4GB RAM without OOM
- Maintain byte-identical output (graph.json, graph.html, Cypher) compared to current pipeline
- Preserve clean architecture boundaries (Domain pure, UseCase pure orchestration, Infrastructure IO)
- Make memory lifecycle explicit and testable

**Non-Goals:**
- On-disk graph storage or SQLite backend (Strategy C — future work)
- Changing the Leiden algorithm or community detection logic
- Modifying LSP extraction protocol or server management
- Adding new export formats
- Changing the MCP server or context selection logic

## Decisions

### Decision 1: Bounded extraction with GC between chunks

**Choice**: Process files in bounded chunks of 500, with `performGC` between chunks.

**Alternatives considered:**
- **STM TBQueue with backpressure**: More elegant, but adds STM complexity and the extraction is already IO-bound (waiting for LSP servers). The chunk approach is simpler and achieves the same memory bound.
- **No chunking (current)**: Files processed via `mapConcurrently` with `QSemN` throttling. `QSemN` limits concurrency but not memory — all results accumulate in `IORef`s.

**Rationale**: The `IORef` accumulation pattern already exists and works. Adding chunk boundaries with `performGC` between chunks caps peak extraction memory at ~500 files worth of data plus the accumulated Maps. The accumulated Maps grow monotonically, but individual file contents and LSP buffers are reclaimed between chunks.

### Decision 2: Explicit phase release boundaries in Pipeline

**Choice**: After each pipeline phase, `evaluate` the output and `performGC` to release the previous phase's data.

**Alternatives considered:**
- **Pure streaming (conduit/pipes)**: Would require rewriting the entire pipeline as a streaming computation. Breaks the clean architecture pattern where Domain functions are pure and take/return concrete types.
- **Weak references + finalizers**: GHC's GC is non-deterministic. Can't guarantee timely release.
- **Manual memory management (ForeignPtr, malloc)**: Unidiomatic for Haskell, error-prone, violates the principle of leveraging GHC's GC.

**Rationale**: `evaluate` + `performGC` at phase boundaries is the simplest approach that works with GHC's lazy GC. After building the Graph, we `evaluate (gNodes graph) >> evaluate (gEdges graph) >> performGC` to force the Graph into memory and let the GC reclaim the Extraction Maps. This is already done partially (line 199 of Pipeline.hs uses `deepseq`), but at the wrong point — it forces before Leiden, keeping both structures live.

### Decision 3: Remove duplicate JSON write

**Choice**: Remove `ExportJSON.exportGraph` call from `exportAll`. The `IncrementalJSON` writer in Pipeline already produces `graph.json`.

**Alternatives considered:**
- **Keep both writes, add flag**: Confusing, two sources of truth for the same file.
- **Switch to IncrementalJSON only for large graphs**: Adds complexity for no benefit — the incremental writer produces identical output.

**Rationale**: The `IncrementalJSON` writer already writes nodes, edges, communities, cohesion, and god nodes to `graph.json` incrementally during the pipeline. The `ExportJSON.exportGraph` call in `exportAll` rewrites the entire file in memory with `encode (object ...)`, which constructs the full JSON AST before writing. This is purely redundant and doubles peak memory during export. The only missing piece is community labels — add `writeAnalysisTail` call for labels in Pipeline.

### Decision 4: Stream HTML export

**Choice**: Rewrite `exportHTML` to stream nodes and edges to file one-by-one using Handle-based IO, instead of building the entire HTML string in memory.

**Alternatives considered:**
- **Builder pattern (Data.Text.Lazy.Builder)**: Still builds the full string in memory before writing. Reduces allocation churn but not peak memory.
- **Temp file + concatenation**: Write header to file, then append nodes, then edges, then footer. Same I/O pattern but more complex.
- **Keep current approach**: On 100k-node graphs, `nodesJSON` alone is 50-100MB of Text. Unacceptable.

**Rationale**: The HTML export encodes nodes and edges as JSON arrays embedded in a vis.js template. Each element can be encoded independently. Use `Data.Aeson.encode` on individual `Node`/`Edge` values and write them to a `Handle` with comma separators. The `IncrementalJSON` writer already demonstrates this pattern — extend it to HTML.

### Decision 5: Stream Cypher export (Neo4j + Memgraph)

**Choice**: Write Cypher statements to file incrementally using Handle IO, instead of building the full `Text` in memory.

**Alternatives considered:**
- **Keep current approach**: `generateCypher g` builds the entire Cypher script as a single `Text` value. On 100k nodes, this is 50-100MB.
- **Batch generation**: Generate in batches of 1000 statements, append to file. More complex but bounds memory.

**Rationale**: The push-to-Neo4j path already uses batching (50 statements per HTTP request). Only the file export path (`exportCypher`) builds the full string. Change it to stream to a `Handle`. Same pattern for Memgraph.

### Decision 6: Release CachedFGL after analysis

**Choice**: Compute all analysis results (articulation points, god nodes) from `CachedFGL`, then let it be GC'd before exports begin.

**Alternatives considered:**
- **Keep CachedFGL alive**: It's referenced in the `Analysis` record, so it stays live until exports finish. On 100k-node graphs, this is ~200MB.
- **Compute analysis lazily**: Would require changing the `Analysis` data type to support lazy fields. Adds complexity.

**Rationale**: The `Analysis` record currently holds `CachedFGL` indirectly (via articulation points and god nodes). The fix: compute all analysis results, store only the derived data (lists of NodeIds, GodNodes), and let the CachedFGL and its Patricia Tree be reclaimed. The `Analysis` type already stores `analysisGodNodes`, `analysisCommunities`, etc. — it doesn't need `CachedFGL` after the computation.

## Risks / Trade-offs

| Risk | Mitigation |
|------|-----------|
| `performGC` adds latency between phases | `performGC` is ~10-50ms. On a pipeline that runs for minutes, negligible. Only called 3-4 times total. |
| Chunked extraction may slow down LSP servers | LSP servers already process one file at a time (didOpen → symbols → didClose). Chunking just adds GC pauses between groups. |
| Streaming HTML changes the output format | Must verify byte-identical JSON arrays. Test by comparing output of current vs streaming on a small codebase. |
| Removing duplicate JSON write changes write timing | `graph.json` will now be complete after the incremental writer closes (during clustering), not after export. Timing changes but content is identical. |
| `evaluate` + `performGC` doesn't guarantee immediate release | GHC's GC is generational. `performGC` does a major GC, which reclaims dead references. This is reliable for our use case — we're releasing entire data structures, not chasing pointer graphs. |

## Verification Strategy (Check)

1. **`cabal build`**: Must compile without warnings (`-Wall -Wcompat -Werror` with `--flag dev`)
2. **`cabal test`**: All existing Hspec + QuickCheck tests must pass
3. **Byte-identical output**: Run `graphos` on a test codebase before and after changes. Diff `graph.json`, `graph.html` (node/edge data), and Cypher output. Must be identical.
4. **Memory profiling**: Run with `+RTS -s` before and after. Measure peak memory on test codebases of 1k, 10k, and 50k files. Target: 50%+ reduction in peak memory.
5. **No regression in node/edge counts**: Verify extraction produces same number of nodes and edges.
6. **OOM test**: Run on a codebase that previously crashed. Verify it completes successfully.

## Iteration & Rollback (Act)

If peak memory does not drop to ≤3× target:
- Profile with `+RTS -h` to identify remaining retention points
- Add more `evaluate` + `performGC` boundaries at phase transitions
- Consider Strategy C (on-disk graph) for next PDCA cycle

If streaming HTML produces different output:
- Fall back to current approach for HTML, keep other streaming changes
- Investigate JSON serialization differences (whitespace, key ordering)

Rollback strategy:
- All changes are additive (new streaming functions, GC boundaries, removed duplicate write)
- Git revert is clean — no database migrations, no config changes
- The `IncrementalJSON` writer remains; only its usage changes

## Open Questions

- Should we add a `--max-memory` CLI flag that sets chunk size and GC frequency? Useful for very constrained environments.
- Should the streaming HTML writer also stream the vis.js JavaScript, or just the data? Currently the HTML is self-contained with inline JS.
- The `runIncrementalPipeline` (watch mode) uses a different extraction path. Should it also get chunked extraction, or is it fine since it only re-extracts changed files?