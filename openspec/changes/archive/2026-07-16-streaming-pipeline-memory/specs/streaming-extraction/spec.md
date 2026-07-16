## ADDED Requirements

### Requirement: Bounded-memory file extraction with chunk-based processing

The extraction phase SHALL process files in bounded chunks. After each chunk completes and its results are accumulated into the shared Maps, the system SHALL perform a major garbage collection to reclaim file content buffers and LSP response data from completed files. The chunk size SHALL be configurable (default: 500 files). Each extraction result SHALL be fully evaluated (`evaluate`) before accumulation to prevent thunk chains across chunks.

#### Scenario: Extraction of 10k files completes without OOM
- **WHEN** Graphos processes a codebase with 10,000 files on a machine with 4GB RAM
- **THEN** the extraction phase SHALL complete without OOM crash, and peak memory during extraction SHALL NOT exceed 2× the final accumulated Extraction Maps size

#### Scenario: Chunk boundaries trigger garbage collection
- **WHEN** a chunk of 500 files finishes extraction and accumulation
- **THEN** the system SHALL call `performGC` to reclaim memory from completed file buffers, and the next chunk SHALL begin with reduced heap pressure

#### Scenario: Extraction results are identical regardless of chunk size
- **WHEN** Graphos processes a codebase with chunk size 500 and then with chunk size 100
- **THEN** the resulting Extraction (nodes and edges) SHALL be identical in both runs

#### Scenario: Single-threaded extraction still works
- **WHEN** `cfgThreads` is 1 (single-threaded mode)
- **THEN** chunk-based extraction SHALL still function correctly, processing files sequentially within each chunk and performing GC between chunks

### Requirement: Incremental JSON write eliminates duplicate full-graph serialization

The pipeline SHALL write `graph.json` exclusively through the `IncrementalJSON` writer. The `ExportJSON.exportGraph` call in `exportAll` SHALL be removed. If community labels are generated (via `--label` flag), they SHALL be written via `writeAnalysisTail` on the existing `IncrementalWriter` before closing.

#### Scenario: graph.json is written once incrementally
- **WHEN** the pipeline completes the build phase
- **THEN** `graph.json` SHALL be written incrementally (nodes, edges) and SHALL NOT be rewritten by `exportAll`

#### Scenario: Community labels appear in graph.json when --label is used
- **WHEN** the `--label` flag is enabled
- **THEN** community labels SHALL be included in `graph.json` via `writeAnalysisTail` on the incremental writer

#### Scenario: graph.json content is byte-identical to previous output
- **WHEN** comparing the output of the streaming pipeline to the previous batch pipeline on the same codebase
- **THEN** the `graph.json` files SHALL contain the same nodes, edges, communities, and cohesion data (order-independent comparison permitted for JSON arrays)