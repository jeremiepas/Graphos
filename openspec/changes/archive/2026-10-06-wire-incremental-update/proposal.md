## Why

Graphos ships three incremental-update mechanisms that were built and never connected: the `--update` flag is parsed into `cfgUpdate` and read by nothing, the SHA-256 content-addressed extraction cache (`Infrastructure.FileSystem.Cache`) has zero callers, and the embedding cache is invalidated by the staged-rebuild swap on every full run. Meanwhile the shipped agent skills and git hook instruct agents and commits to run `graphos . --update`, which today silently performs a full rebuild — re-extracting every unchanged file and re-embedding every unchanged text on each invocation (AVI-521 §1.3, hot-reload cycle cost). This change wires the existing cache machinery into the pipeline so incremental refreshes skip unchanged work, bounded by the content-addressed cache invariants already specified in `02-incremental-pipeline` (PRD §3.4, workflow 02) and formalized in AVI-521 (INV-CACHE-SOUND).

## What Changes

- **Wire the extraction cache into `extractAll`**: per-file SHA-256 hash check at the extraction boundary; cache hit → reuse cached `Extraction`, miss → extract and `saveCached`. The cache is always-on; `--fresh` (`cfgFresh`) bypasses both caches for a cold rebuild.
- **Extend the extraction cache key** with a config fingerprint (granularity, extractor mode, pdf extraction level) so a config change invalidates stale entries, mirroring how the embedding cache keys on model (AVI-521 §1.2: cache key must factor through content *and* extraction-affecting configuration).
- **Fix cache persistence across the staged rebuild**: carry `cache/` into the staging directory *before* the pipeline body runs, so a rebuild reads a warm cache instead of re-embedding everything, and the post-swap carry-over no longer silently fails.
- **Respect `--update`**: the flag becomes meaningful — an update run is a full-pipeline rebuild whose extraction (and embedding) stages are cache-accelerated. The graph is always built from the current file set, so deleted files disappear and communities remain identical to a full build (confluence by construction, AVI-521 §6).
- **Bound cache growth**: LRU size-capped eviction (mtime-based, single readdir sweep at pipeline start), cap configurable in `graphos.yaml` (`0` = unlimited). Eviction is always sound because both caches are content-addressed — an evicted entry is a miss, never a wrong result.
- **Watch mode shares the machinery**: `extractChangedFiles` writes through to the extraction cache, and the incremental path runs `generateGraphEmbeddings` over the merged graph so changed nodes gain embeddings while unchanged texts are disk hits.

No breaking CLI changes: `--update`, `--fresh`, and `--watch` keep their current flags; only their behavior gains the caching they always implied.

## Capabilities

### New Capabilities
- `incremental-cache-eviction`: Bounding the persistent extraction/embedding cache growth via LRU size-cap eviction, configurable in graphos.yaml; eviction is sound (a cache miss re-derives, never a wrong result).

### Modified Capabilities
- `02-incremental-pipeline`: The "Workflow 02 — incremental pipeline with --update" requirement gains real semantics: the extraction cache is wired into `extractAll`, `--fresh` bypasses caches, and unchanged files are skipped on re-run (hit-correctness property H, AVI-521 §1.2).
- `03-watch-mode`: Watch-mode incremental runs write through to the extraction cache and generate embeddings for changed nodes via the content-addressed embedding cache, closing the gap where watch updates never produced vectors.
- `embedding`: The embedding cache is persistent across staged rebuilds (carry-over before body), and embedding incrementality applies to the incremental path, not only full rebuilds.

## Impact

- **UseCase**: `Pipeline/Core.hs` (extractAll cache wiring, embedding cache root handed warm), `Pipeline/Incremental.hs` (embeddings on merged graph), `Extract/Core.hs` (cache check/save at per-file boundary), `Pipeline/Staging.hs` (cache carry-over moves before body).
- **Infrastructure**: `FileSystem/Cache.hs` (config-fingerprinted key), `LLM/EmbeddingCache.hs` (unchanged key scheme, now persistent), new eviction sweep in cache module; `Config` for the eviction cap setting.
- **Domain**: none — cache policy is orchestration/infrastructure; no Domain types change.
- **Systems**: `graphos.yaml` gains one setting (cache eviction cap, default 512MB). Agent skills and git hook documentation become truthful without edits.
- **Formal grounding**: implements INV-CACHE-SOUND / Theorem 1 (hit-correctness) and the §4–5 cost model of AVI-521; confluence of update runs with full builds follows from building on the current file set (§6).