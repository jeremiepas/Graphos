## Purpose

Keep the `--embed` pipeline path within bounded memory: embedding vectors are
stored in a compact unboxed representation, the node-to-vector assignment is
assembled as a single live copy via the streaming sidecar path, and the vector
table is released after its last consumer so peak memory no longer scales as a
multiple of the full embedding set.

## ADDED Requirements

### Requirement: Unboxed in-memory embedding vectors

The system SHALL store in-memory embedding vectors as unboxed 64-bit-float
vectors rather than boxed lists, for every stage that holds them: the batched
client result, the content-addressed cache read/write boundary, the
node-to-vector assignment, and `gEmbeddings` on the graph. The persisted wire
formats (the `embeddings.json` sidecar, cache entry files, and `graph.json`/
`index.json` embedding payloads, if any) SHALL remain arrays of JSON numbers,
byte-compatible with outputs produced before this change.

#### Scenario: Vectors are compact in memory
- **WHEN** the embedding pass holds N vectors of dimension d in memory
- **THEN** the storage cost per vector is O(d) with a small constant
  (one machine double per element, no per-element heap object)

#### Scenario: Wire format unchanged
- **WHEN** a graph is embedded and the `embeddings.json` sidecar is written
- **THEN** the sidecar is a JSON object mapping node ids to arrays of numbers,
  identical in content to the sidecar a pre-change run produces for the same
  texts and model

### Requirement: Single-copy streaming assignment

The embedding pass SHALL assemble the node-to-vector assignment incrementally:
each completed batch's vectors SHALL be appended to the staged sidecar and
folded into the running assignment without ever materializing a second complete
copy of the embedding set (no full fresh-table, no full union table). The peak
number of live vector copies SHALL be at most the final assignment plus the
in-flight batch. The historical write-at-end sidecar mode SHALL be removed;
the streaming staged-write path SHALL be the only sidecar write mode.

#### Scenario: No duplicate full-table intermediates
- **WHEN** the embedding pass completes on a graph whose unique texts exceed
  the configured batch size
- **THEN** the pass never allocates an intermediate map holding all fresh
  vectors separately from the final assignment (verified by inspection + test)

#### Scenario: Assignment equals sequential baseline
- **WHEN** the same graph, texts, model, and deterministic API are run through
  the new assignment assembly and the sequential one-text-per-call baseline
- **THEN** every node receives the same vector as the baseline, including
  which nodes lack a vector after per-input failures

#### Scenario: Cache-hit nodes receive vectors without API calls
- **WHEN** a second run re-embeds texts already in the content-addressed cache
- **THEN** those nodes receive the cached vectors and no API call is issued,
  exactly as before

### Requirement: Embedding table released after last build-time consumer

The pipeline SHALL detach the in-memory embedding table from the graph after
the semantic-edge inference pass has consumed it, so that clustering, analysis,
and export stages do not retain the full vector set. Later stages that need
vectors SHALL load them from the sidecar (following `embeddings_path`) rather
than from the resident graph. The persisted `graph.json` SHALL still carry the
`embeddings_path` pointer so downstream consumers can re-load embeddings.

#### Scenario: Vectors not resident during clustering
- **WHEN** the pipeline runs with `--embed` and reaches the clustering stage
- **THEN** the in-memory embedding table is no longer held by the pipeline
  (the graph's embedding field is empty), while `embeddings.json` remains
  complete and `graph.json` still points at it

#### Scenario: Downstream loads still work
- **WHEN** a query or MCP consumer loads the produced `graph.json` with its
  `embeddings_path` pointer
- **THEN** embeddings load from the sidecar as before (unchanged behavior)

### Requirement: Bounded peak vectors in semantic inference

The semantic code↔doc inference pass SHALL stream doc vectors one doc at a
time against the code table, so its peak live vector count is bounded by one
doc row plus the code table, independent of the number of doc nodes. The pass
MUST NOT force the entire embedding table into memory as a list before
matching, and its output edges SHALL be identical to the all-pairs formulation
for the same inputs (order-insensitive dedup applies).

#### Scenario: Peak live vectors bounded during inference
- **WHEN** semantic inference runs over D doc nodes and C code nodes
- **THEN** at most one doc vector plus the code table is live at any moment
  (not D+C full rows materialized as forced thunks)

#### Scenario: Edge output identical to all-pairs baseline
- **WHEN** the streaming formulation runs on a mixed doc/code graph with
  embeddings
- **THEN** the emitted `References` edges equal (as a set, up to the existing
  dedup and sort key) those the all-pairs formulation emits

### Requirement: Memory-bounded embedding pass accounting

The embedding pass SHALL complete within bounded peak memory: for a graph with
N nodes and dimension d, peak extra memory attributable to the embedding pass
(vectors, assignment, sidecar staging) SHALL be O(N·d) with a small constant
— at most the final assignment plus one in-flight batch — and SHALL NOT scale
as a multiple (2× or more) of the final assignment. A regression test SHALL
assert this bound via RTS live-bytes measurement on a synthetic graph at
sufficient scale to distinguish the bounds (thousands of nodes).

#### Scenario: Peak memory within a small multiple of the assignment
- **WHEN** the embedding pass runs on a synthetic graph of ≥10,000 nodes with
  256-dimension vectors and RTS statistics are compared between the embedding
  pass and a pass that only builds the final assignment
- **THEN** peak live bytes during embedding exceed the assignment-only figure
  by less than the size of one batch of vectors plus a fixed small constant

#### Scenario: Bounded run under a heap cap
- **WHEN** the pipeline runs with `--embed` and `--max-heap 2G` on a synthetic
  50,000-node graph with 256-dimension vectors
- **THEN** the run completes without heap-overflow failure