## Why

Graphos crashes with OOM on large codebases (100k+ files, multi-language). On a 64GB machine, the runtime process exhausts all available RAM during the extract→build→cluster pipeline. The root causes are:

1. **LSP server processes**: Each language server (HLS, typescript-language-server, rust-analyzer, etc.) consumes 1-4GB. Running them concurrently via `mapConcurrently` multiplies this — 5 language servers = 10-20GB just for subprocess memory.

2. **Unbounded accumulation in extraction**: Eight `IORef` accumulators (4× `Map NodeId Node`, 4× `[Edge] -> [Edge]` diff lists) grow simultaneously and can't be GC'd incrementally. Diff lists form O(n) closure chains.

3. **Graph duplication during clustering**: Edge inference creates a new Graph via `buildGraphFromExtractions`, holding the original Graph, the intermediate Extraction, and the enriched Graph simultaneously — triple the peak memory.

4. **Observability grows forever**: `tracerSpans :: IORef [Span]`, `msHistograms :: IORef (Map k [Double])`, and `dtBuffer :: MVar [Text]` are append-only. On a multi-hour run, these accumulate millions of entries.

5. **Node representation overhead**: Each `Node` has 17 fields (12 `Maybe` wrappers = 24 bytes each), yielding ~400 bytes per node before `nodeExtra`. Combined with `Map` overhead (~4×), 1M nodes ≈ 1.6GB.

This makes Graphos unusable on real-world codebases — precisely the use case it was designed for.

## What Changes

Seven targeted fixes to reduce peak runtime memory from 40-60GB+ to under 8GB on large codebases:

1. **Sequential LSP extraction** — Extract LSP file groups one at a time instead of `mapConcurrently`, capping concurrent LSP processes at a configurable limit (default 2). Each group connects, extracts, disconnects before the next starts.

2. **Replace diff-list accumulators with Map unions** — Change `[Edge] -> [Edge]` accumulators to `IORef (Map EdgeId Edge)`, enabling incremental GC and eliminating closure chain overhead.

3. **Streaming extraction merge** — Merge extraction results batch-by-batch into the final Map, calling `performGC` after each batch, instead of accumulating all results in IORefs then merging at the end.

4. **In-place graph enrichment** — Instead of creating a new Graph via `buildGraphFromExtractions` during edge inference, add edges directly to the existing Graph's adjacency structures, avoiding the 2-3× duplication window.

5. **Bounded observability** — Add eviction to `tracerSpans` (keep last N spans), aggregate histograms instead of accumulating observation lists, and cap `dtBuffer` at a configurable size.

6. **Compact Node representation** — Replace 12 `Maybe` fields with a packed representation using `Data.Text.Short` for labels, a bit-field for flags, and `Value` only when `nodeExtra` is non-null. Drops per-node overhead from ~400 bytes to ~150 bytes.

7. **RTS profiling support** — Add `+RTS -h` and `-s` flags to the CLI for heap profiling, making future memory diagnosis trivial.

## Capabilities

### New Capabilities
- `streaming-extraction`: Batch-by-batch extraction with incremental merge and GC, replacing the all-at-once IORef accumulation pattern
- `bounded-observability`: Capped spans, aggregated histograms, and bounded debug traces to prevent unbounded growth
- `compact-nodes`: Packed Node representation reducing per-node overhead from ~400 to ~150 bytes

### Modified Capabilities
- `extraction`: Sequential LSP extraction with concurrency cap replaces `mapConcurrently` (spec change: extraction is now bounded-memory)
- `graph-enrichment`: In-place edge addition replaces full graph rebuild during inference (spec change: `inferEdges` modifies Graph in-place)
- `pipeline`: RTS profiling flags and memory-aware GC hooks added to pipeline stages

## Impact

- **Code**: `UseCase.Extract` (extraction orchestration), `Domain.Types.Node` (Node type), `Domain.Community` (Leiden), `Infrastructure.Observability` + `Observability.SDK` (metrics/traces), `Domain.Graph.Core` (in-place enrichment), `UseCase.Pipeline` (GC hooks), `app/Main.hs` (RTS flags)
- **API**: `Node` type changes (add `NodeCompact` or change representation) — may break JSON consumers if fields change
- **Dependencies**: Potential new dependency on `Data.Text.Short` (in `text-short` package)
- **Performance**: Peak RAM reduced from 40-60GB+ to estimated <8GB on 100k-node graphs; extraction throughput may slightly decrease due to sequential LSP processing, but wall-clock time should improve (less GC pressure)
- **Compatibility**: JSON output format preserved; Node JSON fields unchanged; internal representation only