## Why

Graphos crashes on large codebases (10k+ files) due to unbounded memory consumption. The pipeline holds all data structures simultaneously — Extraction Maps, Graph, LeidenState, CachedFGL, and export buffers — with no explicit release points between phases. Peak memory reaches 6-10× the final graph size, causing OOM kills on machines with ≤8GB RAM.

The codebase already shows awareness of this problem: `StrictData` pragmas, `deepseq` calls, `IORef` accumulators with DList patterns, `IncrementalJSON` writer, and comments referencing "100k+ nodes", "OOM", and "3-4× memory overhead." These are band-aids on a fundamental architectural issue: the pipeline is "load all → transform all → write all" with no streaming or release-between-phases pattern.

## What Changes

Transform the pipeline from "everything in memory simultaneously" to a phased streaming architecture where each phase's output is written incrementally and its input data is released before the next phase begins.

Specific changes:
1. **Chunk extraction** — process files in bounded batches with `performGC` between chunks to cap extraction memory
2. **Release Extraction after building Graph** — add explicit `evaluate` + `performGC` boundaries at phase transitions
3. **Remove duplicate JSON write** — `exportAll` currently rewrites `graph.json` that `IncrementalJSON` already produced
4. **Stream HTML export** — encode nodes/edges one-at-a-time to file instead of building entire HTML string in memory
5. **Stream Neo4j Cypher export** — write Cypher statements incrementally instead of building full string
6. **Release LeidenState after clustering** — convert to CommunityMap, then let GC reclaim vectors/IntMaps
7. **Release CachedFGL after analysis** — compute articulation points and god nodes, then discard the FGL structure

## Capabilities

### New Capabilities
- `streaming-extraction`: Bounded-memory file extraction with chunked processing and GC between batches
- `streaming-export`: Incremental export for HTML and Cypher formats, writing data to disk as it's produced rather than building in-memory strings

### Modified Capabilities
- `pipeline`: Pipeline phases now have explicit memory release boundaries; duplicate JSON write removed; phase transitions use `evaluate` + `performGC`

## Impact

- **UseCase/Pipeline.hs** — Phase boundaries with explicit release points; remove duplicate JSON write
- **UseCase/Extract.hs** — Chunk-based extraction with GC between batches
- **Infrastructure/Export/HTML.hs** — Stream to file instead of building in-memory string
- **Infrastructure/Export/Neo4j.hs** — Stream Cypher to file instead of building string
- **Infrastructure/Export/Memgraph.hs** — Same streaming pattern as Neo4j
- **UseCase/Export.hs** — Remove `ExportJSON.exportGraph` call (IncrementalJSON already writes graph.json)
- **Domain/Community.hs** — No changes (already efficient with Unboxed Vectors)
- **Domain/Graph/Analysis.hs** — No changes (CachedFGL already shared)

## PDCA Cycle

- **Plan**: Reduce peak memory from 6-10× to 2-3× final graph size. Target: process 100k-node graphs on 4GB RAM without OOM. Measured via GHC heap profiling (+RTS -s) on test codebases of 10k, 50k, and 100k nodes.
- **Do**: Implement streaming extraction, streaming exports, phase release boundaries, and remove duplicate JSON write. See design.md for architecture and tasks.md for implementation steps.
- **Check**: Run `cabal test` (all existing tests pass). Run GHC heap profiling before/after on test codebases. Verify graph.json, graph.html, and Cypher outputs are byte-identical to pre-change outputs. Verify no regression in extraction node/edge counts.
- **Act**: If peak memory drops to ≤3× target, standardize the phase-boundary pattern. If streaming HTML proves too complex for the vis.js template, fall back to bounded-buffer approach. Feed findings into next cycle for on-disk graph (Strategy C from exploration).