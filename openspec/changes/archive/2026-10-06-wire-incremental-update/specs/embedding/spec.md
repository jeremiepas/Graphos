## ADDED Requirements

### Requirement: Embedding incrementality on the incremental path

When the incremental pipeline (--watch change handler) runs with embeddings enabled, the system SHALL generate embeddings for the merged graph through the content-addressed embedding cache: only texts not present in the cache SHALL be sent to the embedding API, and every vector (cached or fresh) SHALL be written back to the persistent cache. (PRD §3.4, workflows 02–03; AVI-521 §1.2)

#### Scenario: Watch update embeds only new texts
- **WHEN** an incremental run merges a changed file into a graph whose existing node texts are all in the embedding cache
- **THEN** only the changed file's node texts are sent to the embedding API and the sidecar contains vectors for all nodes

#### Scenario: Full rebuild embeds only misses
- **WHEN** a full rebuild runs with `--embed` after a previous run populated the embedding cache
- **THEN** texts present in the cache are not re-embedded; only new or changed texts produce embedding API calls