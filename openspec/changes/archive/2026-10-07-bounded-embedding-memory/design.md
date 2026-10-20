## Context

The embedding pass (`UseCase.Pipeline.Core.generateGraphEmbeddings`) currently
holds every vector as a boxed `[Double]` and materializes several full copies of
the embedding set in sequence: `cachedTable` (all unique texts' cached vectors),
`freshTable` + `vecTable` (fresh vectors, then a `Map.union` copy), and the
final `assignment` (`Map NodeId [Double]`), which is then stored as
`gEmbeddings = Just embs` on the graph and held live through clustering
(`clusterGraph`), inference, analysis, and export. At 100k nodes × 768 dims,
one boxed copy is ~3-4 GB (cons cells + boxed `Double` heap objects + `Map`
overhead); the pass peaks at 15-20 GB. `fix-runtime-ram-crash` bounded the
extraction/observability/Node paths but did not touch the embedding path.

Constraints that shape the design:

- **Wire formats are frozen**: `embeddings.json` sidecar, cache entry files
  (`Infrastructure.FileSystem.EmbeddingCache`), and any embedding payloads in
  `graph.json`/`index.json` are JSON arrays of numbers. Downstream consumers
  (query, MCP, Neo4j/Memgraph export, Obsidian) read these; none may change.
- **Clean Architecture**: Domain has zero IO; the vector type lives in
  `Domain.Types`/`Domain.Graph` (`gEmbeddings :: Maybe (Map NodeId [Double])`)
  and is consumed by `UseCase.Infer`, `UseCase.Query`, `UseCase.IngestIndex`,
  `UseCase.Port.LLMPort` (`cosineSimilarity`), and `Domain.SpecCheck`
  (its own local `cosineSimilarity` mirror).
- **Existing Lean artifact**: `embedding-throughput` carries a machine-checked
  equivalence proof (`lean/`) for dedup + prepare + batch + cache + streaming
  sidecar. The restructure must stay inside the theorem's model or the proof
  must be updated to match.
- **RTS tooling exists**: `--rts-profile` / `--max-heap` flags from
  `fix-runtime-ram-crash` (task 1) give us heap measurement and a hard cap for
  regression tests.

## Goals / Non-Goals

**Goals:**

- Cut peak `--embed` memory from a multiple (4-5×) of the embedding set to a
  single live copy plus one in-flight batch.
- Make the compact representation the type-level default so future call sites
  cannot silently reintroduce boxed lists.
- Keep every persisted format and the sequential-equivalence guarantee intact.

**Non-Goals:**

- No ANN index or approximate search for semantic inference (the existing 10k
  scale guard and `--force-semantic-edges` stay; a follow-up change may add
  ANN).
- No change to the batch/concurrency/transport behavior or the disk cache
  layout (`EmbeddingCache` keys and files are untouched — only the in-memory
  type crossing its API).
- No float16/quantization of stored vectors (JSON sidecar stays 64-bit).
- No lazy-graph tricks in other pipeline stages; this change is scoped to the
  embedding path and its immediate consumers.

## Decisions

### D1 — Unboxed `Data.Vector.Unboxed.Vector Double` as the in-memory vector type

Replace `Map NodeId [Double]` with `Map NodeId (VU.Vector Double)` everywhere
in memory: `gEmbeddings`, the batched LLM port, the cache boundary, the
assignment, and `cosineSimilarity`.

- ~5× smaller per vector (8 B/dim vs ~40-48 B/dim boxed), cache-friendly.
- `Vector.Unboxed` has an `NFData` instance and no per-element heap object, so
  GC pressure drops proportionally.
- **Alternatives considered:**
  - *Keep `[Double]`, just remove copies* — insufficient: the single final
    copy is still ~3-4 GB at 100k×768, and cons-cell overhead makes every
    downstream cosine scan slower.
  - *`Data.Vector.Storable`* — equivalent size; `Unboxed` is the project's
    existing convention (Leiden uses `VU.Vector`) and needs no ForeignPtr
    lifecycle reasoning.
  - *`text-short`-style compaction of the map keys* — out of scope; `NodeId`
    interning is a separate concern.

### D2 — Single-copy assembly via the streaming sidecar path

Remove the `embStreaming == False` branch. `generateGraphEmbeddings` becomes:

1. Dedup texts; cache lookups as today, but decode straight into `VU.Vector`.
2. Cache-hit entries fold into a running `IORef (Map NodeId (VU.Vector Double))`
   assignment and append to the staged sidecar (existing `writeEntries`).
3. Each batch, as it completes, folds its nodes' vectors into the running
   assignment and appends to the staged file; no `freshTable`/`vecTable`
   intermediate is built. Cache write-back moves per-batch
   (`writeBackFresh` per chunk) so the fresh table is never assembled whole.
4. Final `assignment = readIORef runningRef`; atomic rename as today.

The assignment map is the single full copy; the batch is the only other live
set. The existing D2 bisection / D5 read-only-hits / D9 staged-rename
invariants are preserved verbatim — they live inside `embedChunk` and the
bracket, which don't change.

- **Alternatives considered:**
  - *Keep write-at-end mode behind the config flag* — two code paths to
    maintain and the flag's only remaining purpose was memory; removing it
    simplifies (spec: `streaming` key ignored for compatibility).
  - *Builder/catenable queue then one freeze at the end* — still holds a second
    structure equal in size to the assignment; no better than folding
    directly.

### D3 — Release `gEmbeddings` after semantic inference

After `inferSemanticEdgesForMode` consumes the table in the pipeline
(`Pipeline.Core` step 5, and the same ordering in
`Pipeline/Incremental.hs`), set `graph' = graph { gEmbeddings = Nothing }`
before `clusterGraph` runs, and write `embeddings_path` from the sidecar
variable instead of `gEmbeddingsPath enrichedGraph`. Consumers that need
vectors later load via the loader (sidecar pointer), which already exists and
is spec'd (`semantic-edge-inference`: "Embeddings persisted to graph output
sidecar"). The checkpoint written before clustering (`epSaveCheckpoint`) keeps
its pointer field populated from the sidecar path so `--cluster-only`/
resume runs can still find it.

- **Alternatives considered:**
  - *Keep vectors resident and stream to consumers on demand* — every consumer
    (Leiden, export) would need re-plumbing for marginal benefit; the table is
    only used by semantic inference at build time.
  - *Drop vectors immediately after generation, re-read sidecar for inference*
    — an extra full parse of the sidecar mid-pipeline; the table is already
    in hand right after generation, use it there once, then release.

### D4 — Streaming semantic inference

Rewrite `inferSemanticCodeDocEdges`'s core loop to force one doc row at a
time (`foldM` over doc nodes with a strict accumulator of accumulated edges),
keeping the code table as a `Map` that is referenced but never fully forced
into a list. Cosine scores use the unboxed `cosineSimilarity`
(`VU.foldl'`-based; `LLMPort` version becomes the single implementation —
`Domain.SpecCheck` keeps its local mirror since Domain cannot import UseCase).
Sorting/fan-out logic is unchanged; output is proven-equal by an existing
property test updated for the new type.

- **Alternatives considered:**
  - *Precompute normalized code vectors once* — a good CPU optimization but
    orthogonal; noted as follow-up, not blocking.
  - *Chunk docs in groups of K* — adds tuning knobs for no spec-visible
    benefit; one-at-a-time is strictly bounded.

### D5 — Regression tests measure, not assume

Two test layers:

1. **Equivalence/behavior** (Hspec): assignment equals sequential baseline
   (extends the existing `PipelineSpec`/`IngestSpec` fixtures to the new
   type); legacy `streaming: false` key is ignored; sidecar content unchanged;
   semantic edges equal the all-pairs formulation.
2. **Memory bound** (Hspec + RTS): a synthetic ≥10k-node/256-dim graph run
   with `+RTS -s` captured stderr parses peak live-bytes; the embedding pass
   must stay under assignment-plus-one-batch (fixed tolerance). Skipped when
   RTS stats are unavailable (e.g., non-`-with-rtsopts` test binary) so CI
   environments without the flag don't false-fail. A `--max-heap 2G` run on a
   synthetic 50k-node graph guards the regression end-to-end in dev
   (documented, not CI-gated — CI runners may lack the headroom).

- **Alternatives considered:**
  - *Lean proof extension for the memory claim* — Lean proves the functional
    equivalence (already covered); the *bound* is an RTS measurement, not a
    theorem. Keep proof scope unchanged.
  - *Weigh/compare allocation counters only* — brittle across GHC versions;
    live-bytes from `-s` output is stable enough with the tolerance.

## Risks / Trade-offs

- **Type-change ripple** (`[Double]` → `VU.Vector Double` touches ~10 modules
  including tests and possibly `text-short`-adjacent serialization) →
  mechanical, compiler-driven; done in one task with `cabal build -Werror`
  gate before any behavior change.
- **Lean artifact drift**: the restructured fold could diverge from the proven
  formulation → keep `chunkBy`/`dedup`/`writeBackFresh` semantics identical;
  re-run `lake build` in the streaming-embeddings `lean/` directory as a check
  task; only formulation-preserving refactorings are allowed.
- **Per-batch cache write-back changes cache file mtimes order** — cache
  content is content-addressed and order-independent; the "cache hits are not
  rewritten" scenario only forbids rewriting hits, which stays true.
- **Incremental path regression** (`Pipeline/Incremental.hs` uses the same
  `inferSemanticEdgesForMode`) → the incremental pipeline shares the new
  streaming implementation; its spec test (`fix-pipeline-e2e`) must pass
  unchanged.
- **Unboxed vectors and `Aeson`**: sidecar encoding needs an explicit
  `VU.Vector Double -> Value` path (boxed `toJSON` works via `ToJSON` instance;
  verify `ToJSON (VU.Vector Double)` exists in the pinned aeson — else convert
  with `VU.toList` at the write boundary only).
- **Measurement flakiness** in the RTS bound test → generous fixed tolerance
  (one batch + constant), plus `skip` when RTS output is absent.

## Migration Plan

1. Land the type change + single-copy assembly + streaming-only mode in one
   PR (behavior-preserving: equivalence tests must pass before/after).
2. Land `gEmbeddings` release + streaming semantic inference in a second PR
   (or same PR if review prefers; equivalence tests cover both).
3. No config migration needed: absent/legacy `embedding.streaming` keys are
   ignored; no new keys.
4. Rollback: revert is safe — wire formats unchanged, no persisted state
   migration; the only visible difference is lower peak memory.

## Open Questions

None — the batch/concurrency/cache semantics, wire formats, and config
surface are all unchanged by this design.