## Purpose

Bounds the growth of the persistent content-addressed extraction and embedding caches under `graphos-out/cache/` so incremental updates stay fast across sessions without unbounded disk consumption. Eviction is always sound because both caches are content-addressed: an evicted entry is a cache miss that re-derives the same result, never a wrong result (AVI-521 §1.2, INV-CACHE-SOUND).

## ADDED Requirements

### Requirement: LRU size-capped cache eviction

The pipeline SHALL sweep the extraction cache (`cache/`) and the embedding cache (`cache/embeddings/`) at the start of each pipeline run. When the combined size of both caches exceeds the configured cap, the system MUST evict least-recently-used entries (oldest file modification time) until the combined size is at or below the cap, and MUST NOT evict entries touched during the current run. The cap SHALL be configurable in `graphos.yaml` (default 512 MB); a value of `0` MUST disable eviction (unlimited growth). A cache entry touched by a read or write during the sweep's run SHALL have its modification time refreshed so recently used entries survive future sweeps. (PRD §3.4, workflow 02; AVI-521 §1.2)

#### Scenario: Cap exceeded evicts oldest entries
- **WHEN** the combined cache size is 600 MB with the default 512 MB cap and entries with distinct modification times exist
- **THEN** the sweep evicts entries oldest-first until the combined size is at most 512 MB before extraction begins

#### Scenario: Zero disables eviction
- **WHEN** the cache cap is configured as `0` and the combined cache size is 1 GB
- **THEN** no entries are evicted and the pipeline proceeds normally

#### Scenario: Under cap is a no-op
- **WHEN** the combined cache size is 100 MB with the default 512 MB cap
- **THEN** the sweep evicts nothing and reports zero evictions

#### Scenario: Eviction never produces wrong results
- **WHEN** an extraction cache entry is evicted and the same unchanged file is processed in the next run
- **THEN** the file is re-extracted and the resulting graph is identical to the cached-path result (hit-correctness property H, AVI-521 §1.2)

#### Scenario: Missing cache directory
- **WHEN** no `cache/` directory exists on pipeline start
- **THEN** the sweep is a no-op and the pipeline creates the directory on first cache write