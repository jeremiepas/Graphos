## Context

Three incremental mechanisms exist but are disconnected (see proposal.md — Why): `cfgUpdate` has zero readers, `Infrastructure.FileSystem.Cache` (`loadCached`/`saveCached`) has zero callers, and the staged-rebuild swap (`UseCase.Pipeline.Staging`) carries `cache/` into staging only *after* the pipeline body already wrote a fresh `staging/cache`, so the carry-over silently fails and the swap discards the old cache. The embedding cache (`Infrastructure.LLM.EmbeddingCache`) is already content-addressed on `(model, text)` and already used miss-only by `generateGraphEmbeddings` (`UseCase.Pipeline.Core`); it merely never survives a rebuild. The existing incremental path (`UseCase.Pipeline.Incremental.runIncrementalPipeline`, used by `--watch`) never embeds at all.

Constraints from the formal model (AVI-521): the extraction cache is sound iff the key factors through file content and extraction-affecting configuration (§1.2, INV-CACHE-SOUND); update runs must be confluent with full builds (§6). The current `Cache.hs` key is content-only — unsound under config changes — so it must be extended before it is wired in.

## Goals / Non-Goals

**Goals:**
- `--update` performs a cache-accelerated rebuild: unchanged files skip extraction, unchanged texts skip embedding, graph built from the current file set.
- Cache state persists across staged rebuilds (both extraction and embedding caches).
- `--fresh` bypasses both caches.
- Cache growth bounded by LRU size-cap eviction, configurable.
- `--watch` incremental runs write through to the extraction cache and gain embeddings for changed nodes.

**Non-Goals:**
- No change to Leiden clustering, inference, analysis, or export — they run fully in both paths by design (confluence).
- No partial/incremental clustering or community reuse.
- No cross-project or shared cache; the cache remains per-output-directory state.
- No change to checkpoint resume (already orthogonal and working).
- Manifest (`FileSystem/Manifest`) stays dead; wiring the content cache supersedes it.

## Decisions

### D1: Cache-accelerated full rebuild over retained-graph merge

The `--update` run executes the full pipeline; only the extract (per-file) and embedding (per-text) steps consult caches.

```
 detect (full, cheap: readdir + stat)
    |
    v
 extract:  for each file f
             key = sha256(content) <> config-fingerprint
             hit  -> cached Extraction
             miss -> extract -> saveCached
    |
    v
 merge Extractions -> build -> cluster -> infer -> analyze -> export
                     (identical to full build; confluence by construction)
```

**Alternatives considered:**
- *Retained-graph merge* (load old graph.json, `mergeGraphs`, recluster): loses deletions (old nodes win `Map.union`), compounds inferred edges (needs `cleanInferred` band-aid), community drift vs full build. Rejected — correctness.
- *Sub-graph incremental clustering* (recluster only affected communities): would save Leiden time but breaks confluence with full builds and is a research problem (AVI-529 territory). Rejected — out of scope.

**Layers:** cache lookup/save is Infrastructure (`FileSystem.Cache`); the per-file consult loop is UseCase orchestration (`UseCase.Extract.Core.extractAll` calling through a port); no Domain change.

### D2: Config-fingerprinted cache key

The cache key becomes `sha256(content) <> sha256(fingerprint)` where the fingerprint is a canonical serialization of extraction-affecting config: effective granularity (CLI override + per-extension), extractor mode per extension (tree-sitter/lsp/stub), and pdf extraction level. Same scheme as the embedding cache's model-in-key (`EmbeddingCache.cacheKey` = hash of model <> text).

**Alternatives considered:**
- *Version-stamped directories* (`cache/v2-function/`): coarse — one config change invalidates everything; also multiplies directories. Rejected.
- *Store config in the entry and compare on read*: works, but a read then requires decoding JSON before deciding a hit; key-level separation keeps `doesFileExist` the hit test. Rejected.

Note: old content-only entries simply miss under the new key; no migration needed.

### D3: Carry the cache into staging *before* the body

`withStagedOutput` gains a pre-body step: move (rename) `cache/` (and other `carryOverEntries`) from the existing output into the fresh staging dir before invoking the pipeline body, instead of moving them after the body via `carryOverState`. The body then reads/writes `staging/cache` warm, and the swap promotes it with the run's new entries. On failure the staging dir is deleted — the moved cache would go with it, so the pre-body move must be *copy-then-swap-safe*: rename back on failure (add a rollback in the `Left`/exception path of `withStagedOutput`), or copy instead of rename for `cache/` specifically (cache is re-derivable state, so copy cost is acceptable and failure handling is trivial).

Decision: **rename with rollback** for consistency with the existing swap machinery; `memory/`, `debug/`, `traces/` keep their existing post-body behavior (they are not read during the body).

**Alternatives considered:**
- *Fix only ordering* (keep post-body carry-over but merge directories instead of rename): merging after the body wrote `staging/cache` means unioning thousands of files each run, O(cache) every rebuild. Rejected.
- *Symlink `staging/cache -> ../out/cache`:* crosses the swap boundary; the swap would leave a dangling link. Rejected.

### D4: LRU size-cap eviction

One sweep at pipeline start (before detect), Infrastructure function in `FileSystem.Cache`: `readdir` both `cache/` and `cache/embeddings/`, stat each entry, sort by mtime ascending, evict until `sum(sizes) <= cap`. Cap from `graphos.yaml` (`cache: {max_mb: 512}`, `0` = unlimited), read via the Config loader into `GraphosConfig`. mtime is the LRU clock: cache writes already touch mtime (atomic temp+rename yields current mtime), and the sweep must refresh mtimes of entries read during the sweep itself is unnecessary — entries are only evicted at start, and any entry present at start was written/read in a previous run. The default 512 MB is chosen to hold roughly a full history of the extraction + embedding working set for a mid-sized repository.

**Alternatives considered:**
- *Manifest-referenced GC* (delete hashes not in current file set): evicts the branch-switch working set — precisely the workload `--update` serves. Rejected.
- *Entry-count cap:* embedding entries (~4KB each) and extraction entries (up to ~MBs) differ by orders of magnitude in size; count caps bound the wrong resource. Rejected.
- *No eviction:* unbounded disk growth. Rejected (user decision, Q3).

### D5: Watch mode shares the machinery

`extractChangedFiles` (`UseCase.Extract.Core`) gains write-through: each changed file's extraction is `saveCached` under the same fingerprinted key. `runIncrementalPipeline` gains, when `cfgEmbed` is on, a call to the existing `generateGraphEmbeddings` over the merged graph before export, writing `embeddings.json` via `writeEmbeddingsSidecar`. Unchanged texts hit the persistent cache; only changed nodes' texts hit the API.

**Alternatives considered:**
- *Embed only the changed file's nodes* (delta embeddings, merge into old sidecar): requires loading the old sidecar and trusting its keys; the content-addressed cache already makes full-graph pass cheap (disk hits), and the sidecar is rewritten wholesale anyway. Rejected — the cache *is* the delta mechanism.

### D6: `cfgUpdate` vs always-on

The cache is consulted whenever `cfgFresh` is off — i.e. always-on, including plain `graphos .`. `cfgUpdate` remains a no-op flag semantically (its behavior is now the default); it is kept parsed for compatibility with skills/hooks/docs. Rationale: the git hook and agent skills run `--update` believing it is fast; always-on makes plain rebuilds fast too, and `--fresh` is the explicit escape hatch.

**Alternatives considered:**
- *Gate on `cfgUpdate`:* leaves plain `graphos .` slow and keeps two divergent behaviors to document. Rejected (user decision, Q1: always-on, `--fresh` as escape).

## Risks / Trade-offs

- [Stale cache served after an unfingerprinted config change] → Fingerprint covers every config value that feeds `partitionByExtractor`/granularity/pdf level; any future extraction-affecting config MUST be added to the fingerprint (documented in Cache module header).
- [Rename-back rollback fails (process killed mid-run)] → The old output dir is untouched (rename happened into staging, staging is a sibling); worst case the cache lives in a swept-away staging dir and the next run starts cold — sound, only slow once.
- [mtime-based LRU inaccurate across clock skew / file copies] → Cache entries are local, written by the same machine; skew only reorders evictions, and eviction order never affects correctness.
- [Two concurrent pipelines sharing one output dir] → Pre-existing condition (Staging module declares multi-writer coordination out of scope); the sweep and rename steps inherit the same limitation. No regression.
- [Extraction cache entries are larger than embeddings (MBs per file)] → The 512 MB default cap may thrash on very large repos; cap is configurable and `0` disables. Log eviction counts at INFO.

## Migration Plan

1. Ship cache-key fingerprinting and eviction sweep first (pure Infrastructure, no behavior change until wiring).
2. Wire `extractAll` cache consult + write-through and the staging pre-body carry-over.
3. Wire watch-mode embeddings.
4. Rollback: revert the wiring commits; cache files are inert data (nothing reads them once unwired), so no cleanup is required.

## Verification

- `cabal build` and `cabal test` (Hspec) after each step.
- Unit tests: cache key fingerprint sensitivity (same content, different granularity → different key); hit-correctness (cached extraction equals fresh extraction); eviction sweep (cap exceeded evicts oldest; `0` disables; missing dir is a no-op); staging carry-over (cache present in final dir after swap; failed run leaves old cache intact).
- Confluence test: run `--fresh` full build and `--update` over the same tree; exported `graph.json` node/edge sets equal.
- Manual: `graphos . --embed` twice; second run logs embedding hits only.