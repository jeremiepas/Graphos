## Context

Graphos processes codebases into knowledge graphs via a 7-stage pipeline: detect → extract → build → cluster → infer → analyze → export. On large codebases (50k+ files, multi-language), peak runtime memory exceeds 64GB, causing OOM crashes.

The current architecture assumes all extraction results fit in memory simultaneously. LSP server processes are spawned concurrently with `mapConcurrently`, each consuming 1-4GB. Extraction accumulates results in 8 `IORef` accumulators (4 category groups × 2 accumulators each). The Leiden clustering phase creates intermediate data structures that coexist with the original graph. Observability stores (spans, histograms, debug traces) grow without bound.

Key constraints:
- Domain layer must remain pure (no IO) — memory management belongs in Infrastructure
- UseCase layer orchestrates but delegates IO to Infrastructure
- JSON output format must be preserved for consumers
- Existing test suite must continue passing
- The `--threads` flag controls extraction parallelism and must continue working

## Goals / Non-Goals

**Goals:**
- Reduce peak runtime memory to <8GB on 100k-node, multi-language codebases
- Enable Graphos to run on standard developer machines (16GB RAM)
- Preserve all existing functionality (no feature removal)
- Make memory usage observable via RTS profiling flags

**Non-Goals:**
- Build-time (compilation) memory reduction (separate concern)
- Changing the JSON output schema (consumers depend on it)
- Switching from LSP to tree-sitter-only extraction (LSP is the primary extractor)
- Rewriting the Leiden algorithm (implementation is correct, just needs memory discipline)
- Database-backed storage for the graph (in-memory is fine with proper discipline)

## Decisions

### D1: Sequential LSP extraction with concurrency cap

**Decision**: Replace `mapConcurrently` for LSP file groups with a bounded semaphore pool (max 2 concurrent LSP processes). Each group connects, extracts all files, disconnects before the next group starts.

**Alternatives considered**:
- A: Status quo (`mapConcurrently` for all groups) — OOM on multi-language codebases
- B: Fully sequential (1 LSP at a time) — correct but too slow for large codebases
- C: **Bounded pool (max 2 concurrent)** — balances memory and throughput

**Rationale**: Each LSP server uses 1-4GB (HLS uses 2-8GB). Capping at 2 concurrent servers limits LSP memory to ~8GB while still enabling parallel extraction within each language. The `--threads` flag already exists for extraction parallelism — we extend its semantics to also cap concurrent LSP servers.

**Layer**: UseCase.Extract (orchestration), Infrastructure.LSP.Client (lifecycle)

### D2: Replace diff-list accumulators with Map unions

**Decision**: Change `IORef ([Edge] -> [Edge])` accumulators to `IORef (Map EdgeId Edge)`. Merge with `Map.union` after each batch. This enables incremental GC of processed batches.

**Alternatives considered**:
- A: Keep diff lists — O(n) closure chains, no incremental GC
- B: Use `Seq Edge` — better than diff lists but still no incremental merge
- C: **`IORef (Map EdgeId Edge)`** — O(1) amortized union, enables per-batch GC, deduplicates by construction

**Rationale**: The diff-list pattern `[Edge] -> [Edge]` creates a chain of 100k+ thunks that can't be GC'd until the final `($ [])` evaluation. Using `Map EdgeId Edge` gives O(log n) insertion, O(n+m) union for merging, and deduplicates edges by ID. After each batch, we can `evaluate` and `performGC` to reclaim memory from completed extractions.

**Layer**: UseCase.Extract (accumulation pattern), Domain.Types.Graph (EdgeId key)

### D3: Batch extraction with incremental merge and GC

**Decision**: After each file group completes extraction, merge its results into the final `Extraction` and evaluate+GC. This replaces the current pattern of accumulating all results in IORefs and merging at the end.

**Alternatives considered**:
- A: Accumulate all, merge at end — current pattern, OOM risk
- B: **Batch merge + GC** — merge after each group, evaluate, GC
- C: Stream to disk (JSONL) — requires new file format, breaks current API

**Rationale**: The current `extractAll` runs code+office+doc+image extraction concurrently via `concurrently`, accumulating in 8 IORefs. With batch merge, after each group finishes, we merge its `Extraction` into the running aggregate, evaluate the aggregate size, and call `performGC`. This bounds peak memory to roughly `running_aggregate + one_batch + one_LSP_server`.

**Layer**: UseCase.Extract (pipeline orchestration)

### D4: In-place graph edge enrichment

**Decision**: Add a `addEdges` function to `Domain.Graph.Core` that inserts edges into an existing Graph's adjacency maps without creating a new Graph. Use this during edge inference instead of `buildGraphFromExtractions`.

**Alternatives considered**:
- A: `deepseq` the enriched graph (current) — forces evaluation but both old and new exist simultaneously during construction
- B: **`addEdges` in-place mutation** — adds edges to existing Maps, no duplication
- C: Use mutable graph (ST monad) — breaks pure Domain API, complex to implement
- D: Use `Data.Map.Strict.union` to merge — still creates a new Map temporarily

**Rationale**: The current pattern creates a full `Extraction` from all nodes + all edges + inferred edges, then calls `buildGraphFromExtractions` which creates entirely new Maps. This means the old Graph, the Extraction Maps, and the new Graph all exist simultaneously (~3× memory). With `addEdges`, we only add the inferred edges to the existing adjacency structures. Since `Map.insert` returns a new Map (Haskell immutability), the old Maps can be GC'd as soon as the new references take over — but crucially, we avoid the intermediate Extraction entirely.

**Layer**: Domain.Graph.Core (new `addEdges` function)

### D5: Bounded observability stores

**Decision**: Add capacity limits to `tracerSpans` (keep last 1000 spans), `msHistograms` (pre-aggregate into buckets instead of accumulating `[Double]`), and `dtBuffer` (cap at 10k events, flush to disk).

**Alternatives considered**:
- A: Unbounded (current) — OOM on long runs
- B: **Bounded with eviction** — fixed-size buffers, discard oldest
- C: External storage (Redis/file) — overkill, adds IO dependency to Domain-adjacent code
- D: Disable observability by default — loses debugging capability

**Rationale**: The observability stores are in Infrastructure (IORef, MVar) so capacity limits don't violate the Domain purity rule. Spans are primarily useful for the last N events (for debugging). Histograms should be pre-aggregated (count, sum, min, max, buckets) rather than accumulating raw observations. Debug traces already flush to disk but the in-memory buffer grows between flushes.

**Note**: This change previously targeted both `Infrastructure.Observability` and `Infrastructure.Observability.SDK`; `cleanup-ram-fix-prework` deleted the dead `Observability.hs` module, so D5 now applies only to `Observability.SDK`.

**Layer**: Infrastructure.Observability.SDK

### D6: Compact Node representation

**Decision**: Replace remaining `Maybe` fields in `Node` with a packed representation using a bit-field for presence flags and `Data.Text.Short` (from `text-short` package) for short strings. Keep JSON serialization identical via `ToJSON`/`FromJSON` instances. After `cleanup-ram-fix-prework`, the `Node` record has 12 canonical fields (was 17 with 5 legacy fields removed), so the scope of this change is reduced.

**Alternatives considered**:
- A: Keep current Node (12 fields, 7 `Maybe`) — still ~250-300 bytes/node, 4× Map overhead
- B: **Compact Node with Text.Short + bit-field** — ~100-120 bytes/node, same JSON output
- C: Use `data NodeCore` + `data NodeExtra` split — reduces common-case size but breaks pattern matching
- D: Use `Compact` regions from `compact` package — requires deep changes to graph construction

**Rationale**: Each `Maybe` adds 24 bytes (pointer + tag). The remaining 7 `Maybe` fields (`nodeLineStart`, `nodeLineEnd`, `nodeSignature`, `nodeCommunityId`, `nodeKind`, `nodeDegree`, `nodeIsBridge`, plus the recursive `Maybe Value` for `nodeExtra`) still dominate per-node memory. Using a `Word64` bit-field for presence flags and `Data.Text.Short` for labels, source file, and signature drops per-node overhead further. The JSON output remains identical because `ToJSON`/`FromJSON` instances serialize the same fields regardless of internal representation.

**Note**: `nodeExtraCapturedAt` and `setNodeExtraCapturedAt` helpers introduced in `cleanup-ram-fix-prework` must continue to work with the compact representation (the `nodeExtra` field is still a `Maybe Value`, so the helpers are unchanged).

**Layer**: Domain.Types.Node (internal representation change), Domain.Types (re-exports unchanged)

### D7: RTS profiling CLI flags

**Decision**: Add `--rts-profile` flag that causes the executable to re-exec itself with `+RTS -s -hT` appended, producing GC statistics and heap profile. Also add `--max-heap SIZE` flag that re-execs with `+RTS -M SIZE` to cap heap size.

**Alternatives considered**:
- A: Document `+RTS` flags separately — users don't know to try it
- B: **CLI flags that re-exec with RTS options** — discoverable, no manual needed; GHC freezes RTS options at process start so this is the only way to honor a CLI flag
- C: Always-on heap profiling — overhead on every run

**Rationale**: GHC's RTS options are frozen at process start; `setRTSOpts` does not exist. The executable must re-run itself (`getExecutablePath` + `executeFile` or `rawSystem`) with `+RTS -s -hT` appended after `--`. For `--max-heap SIZE`, re-exec with `+RTS -M SIZE`. This gives users first-class flags without requiring them to know the `-- +RTS` syntax. Heap profiles require a binary built with `-rtsopts`; `-hT` works on standard builds.

**Layer**: app/Main.hs (CLI re-exec logic), Infrastructure layer not needed

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Sequential LSP extraction slower for single-language codebases | `--threads` flag still controls parallelism within each language; only cross-language parallelism is capped |
| `addEdges` requires modifying immutable Maps, still creates new Maps | Haskell immutability means old Maps are GC'd once unreferenced; net effect is 1.5× instead of 3× |
| `text-short` is a new dependency | It's lightweight (one module), widely used, and already on Hackage; no transitive dependency bloat |
| Compact Node changes internal representation | JSON output is identical; only internal memory layout changes; Hspec tests verify round-trip |
| Bounded observability loses old spans | Only the last N spans are useful for debugging; histograms are pre-aggregated (no data loss, just granularity) |
| RTS profiling flags may confuse users | Flags are opt-in and well-documented; `--max-heap` has a clear error message when hit |

## Verification Strategy (Check)

1. **Memory profiling**: Run `graphos . +RTS -s -hT` on a 50k+ file multi-language codebase. Verify peak heap < 8GB via `+RTS -s` output.
2. **Existing tests**: `cabal test` — all Hspec + QuickCheck tests must pass without modification (except Node round-trip tests if representation changes).
3. **Regression test**: Compare output of `graphos .` on a test codebase before/after changes — `graph.json` must be structurally identical (node count, edge count, community count within 1% tolerance for Leiden non-determinism).
4. **Extraction throughput benchmark**: Time a single-language extraction before/after sequential LSP change — must be within 20% of original throughput.
5. **Observability bounds test**: Create a test that generates 10k spans and 10k histogram observations. Verify memory doesn't grow beyond capped limits.

## Iteration & Rollback (Act)

- **If sequential LSP is too slow**: Increase default concurrency cap from 2 to 3, or make it configurable via `--lsp-concurrency N`.
- **If compact Node breaks JSON**: Roll back Node changes, keep other fixes. The Node change is independent.
- **If bounded observability loses debugging data**: Increase span cap from 1000 to 5000, or add `--trace-size N` flag.
- **If any fix causes test failures**: Each fix (D1-D7) is independently deployable. Roll back the failing fix while keeping others.
- **Standardize**: After verification, add memory-aware patterns (bounded buffers, incremental merge, compact types) to `.opencode/context/core/standards/code-quality.md`.

## Dependency Note

This change depends on `cleanup-ram-fix-prework` having landed:
- Dead `Observability.hs` module removed.
- `OtelConfig` moved to `Domain.Config`.
- 5 legacy `Node` fields removed, reducing D6's scope.

## Migration Plan

1. Deploy D7 (RTS profiling) first — no behavior change, enables measurement. **Note**: GHC freezes RTS options at process start; the CLI must re-exec itself with `+RTS -s -hT` appended when `--rts-profile` is passed.
2. Deploy D5 (bounded observability) — no external API change, reduces background growth.
3. Deploy D2+D3 (Map accumulators + batch merge) — same output, less memory.
4. Deploy D1 (sequential LSP) — configurable via `--threads`/`--lsp-concurrency`.
5. Deploy D4 (in-place enrichment) — requires new `addEdges` in Domain.Graph.Core.
6. Deploy D6 (compact Node) — requires `text-short` dependency, test updates.
7. After each step, run `cabal test` + manual memory profiling on target codebase.

Rollback: Each step is reversible via git revert. No database migrations or config changes required.