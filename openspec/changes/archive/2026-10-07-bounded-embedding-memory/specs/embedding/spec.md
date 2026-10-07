## MODIFIED Requirements

### Requirement: Batched embedding API surface

The embedding client SHALL expose a batched operation taking a list of texts and returning either an error or one unboxed vector per input text, in input order. The single-text operation SHALL remain available and MUST be equivalent to a batch of one. Both operations SHALL return the compact unboxed representation used by all in-memory stages.

#### Scenario: Single text via batched API
- **WHEN** the batched operation is called with exactly one text
- **THEN** the result is the same vector the single-text operation returns for that text

#### Scenario: Batched result is the compact in-memory representation
- **WHEN** the batched operation returns vectors for a batch of texts
- **THEN** each vector is the unboxed 64-bit-float representation, not a boxed list

### Requirement: Streaming sidecar write

The embedding pipeline SHALL always use the streaming sidecar write path: vectors SHALL be written to a staged `embeddings.json` incrementally — each completed batch's entries append to the staged file — and the staged file SHALL be atomically renamed into place when the embedding pass completes. The historical write-at-end mode SHALL be removed; `embedding.streaming` SHALL no longer be configurable and a legacy `streaming: false` key MUST be ignored without error. An interrupted run MUST NOT leave a partially written `embeddings.json` at the final path. The node-to-vector assignment SHALL be folded in incrementally during the same pass so no second full copy of the embedding set is materialized (bounded-embedding-memory).

#### Scenario: Interrupted streaming run leaves prior sidecar intact
- **WHEN** a run is interrupted mid-embedding-pass
- **THEN** the previous `embeddings.json` remains valid and complete, and no partially written file exists at the final path

#### Scenario: Streaming writes complete sidecar atomically
- **WHEN** the embedding pass completes
- **THEN** `embeddings.json` is atomically renamed into place and its content equals the sequential baseline's node-to-vector assignment content

#### Scenario: Legacy streaming key ignored
- **WHEN** `graphos.yaml` contains `embedding.streaming: false` from a previous configuration
- **THEN** configuration loading succeeds and the run uses the streaming path anyway (no error, no behavior difference)

#### Scenario: Opt-out preserves today's write-at-end behavior
- **WHEN** a legacy `embedding.streaming: false` key is present in `graphos.yaml`
- **THEN** the key is ignored (the write-at-end mode no longer exists) and the sidecar content is identical to what the streaming path produces, which equals the historical write-at-end content

## REMOVED Requirements

### Requirement: Opt-out preserves today's write-at-end behavior (superseded scenario retained above)
**Reason**: The write-at-end sidecar mode is removed — it materializes a second full copy of the embedding set and was the memory-bound change's target; streaming is now the only sidecar write mode.
**Migration**: Remove `embedding.streaming: false` from `graphos.yaml`; the key is ignored without error and every run uses the streaming staged write with atomic rename, producing identical sidecar content.