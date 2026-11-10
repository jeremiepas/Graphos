## MODIFIED Requirements

### Requirement: Workflow 02 — incremental pipeline with --update

Module `Graphos.UseCase.Pipeline` SHALL provide incremental update behavior for `graphos <path> --update`: the pipeline SHALL run the full stage sequence (detect → extract → build → cluster → infer → analyze → export), with the extraction stage cache-accelerated — for each detected file, the system SHALL compare the file's current SHA-256 content hash plus the run's extraction config fingerprint against the persistent extraction cache under `graphos-out/cache/`; a hit MUST reuse the cached extraction without re-parsing, a miss MUST extract and write the result to the cache. The graph SHALL always be built from extractions of the current file set (cached hits for unchanged files merged with fresh extractions of changed files), so deleted files disappear from the output and the resulting graph is identical to a full build over the same content (confluence, AVI-521 §6). The `--fresh` flag SHALL bypass both the extraction cache and the embedding cache for the run (cache reads skipped, cache writes still permitted). (PRD §3.4, workflow 02)

#### Scenario: Incremental skips unchanged files
- **WHEN** `--update` is used and 95/100 files have matching hashes and config fingerprint
- **THEN** only 5 files SHALL be re-extracted; 95 reuse cached extractions and no tree-sitter/LSP parse is invoked for them

#### Scenario: Incremental with no changes
- **WHEN** `--update` is used and no files changed since the previous run
- **THEN** every file SHALL be served from the extraction cache and the pipeline proceeds through build, cluster, and export without invoking any extractor

#### Scenario: Deleted files disappear
- **WHEN** a file is deleted and `--update` is run
- **THEN** the output graph contains no nodes from the deleted file

#### Scenario: Update run equals full build
- **WHEN** `--update` runs over a tree where some files changed since the previous run
- **THEN** the exported graph.json is identical (same nodes, edges, and communities up to re-clustering determinism) to a `--fresh` full build over the same tree

#### Scenario: Fresh bypasses caches
- **WHEN** `--fresh` is passed and all files are unchanged
- **THEN** every file is re-extracted and every embedding text is re-embedded, ignoring existing cache entries

## ADDED Requirements

### Requirement: Extraction cache key includes config fingerprint

The extraction cache key SHALL be the SHA-256 of the file content concatenated with a fingerprint of the extraction-affecting configuration (granularity, extractor mode per extension, pdf extraction level). Changing any fingerprinted configuration value MUST produce different cache keys, so stale extractions are never served after a configuration change. (PRD §3.4, workflow 02; AVI-521 §1.2 — cache key factors through content and extraction-affecting configuration)

#### Scenario: Granularity change invalidates cache
- **WHEN** the same file content is extracted under granularity `function` and then under granularity `file`
- **THEN** the two runs use different cache keys and the second run re-extracts rather than reusing the first run's entry

#### Scenario: Same config reuses cache
- **WHEN** the same file content is extracted twice with identical extraction configuration
- **THEN** both runs compute the same cache key and the second run is a cache hit

### Requirement: Cache persistence across staged rebuild

The staged full rebuild SHALL carry the persistent cache directory into the staging directory before the pipeline body runs, so that a rebuild reads a warm cache and cache entries written during the run survive the output swap. After a successful swap, the final output directory MUST contain the cache with all entries from before the run plus entries written during the run. (PRD §3.3; AVI-521 §4–5 cost model requires a persistent cache across reload cycles)

#### Scenario: Rebuild reuses previous run's cache
- **WHEN** a full rebuild runs with `--embed` after a previous run populated the cache
- **THEN** unchanged embedding texts are served from the cache and are not re-embedded

#### Scenario: Failed rebuild leaves cache intact
- **WHEN** a rebuild fails after some cache entries were written to staging
- **THEN** the previous output directory and its cache remain unchanged and usable by the next run