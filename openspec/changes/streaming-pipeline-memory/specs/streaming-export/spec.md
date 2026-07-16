## ADDED Requirements

### Requirement: Stream HTML export to file without full in-memory string

The HTML export SHALL encode nodes and edges individually to a file Handle, writing each element's JSON encoding as it's produced. The export SHALL NOT build the entire HTML page as a single `Text` value in memory. Nodes SHALL be encoded one-at-a-time using `Data.Aeson.encode` and written to the Handle with comma separators.

#### Scenario: HTML export on 100k-node graph completes without OOM
- **WHEN** Graphos exports an HTML visualization for a graph with 100,000 nodes
- **THEN** the export SHALL complete without OOM, and peak memory during HTML export SHALL NOT exceed 1.5× the in-memory Graph size

#### Scenario: Streaming HTML produces identical node/edge data
- **WHEN** comparing the streaming HTML output to the previous batch HTML output on the same codebase
- **THEN** the node and edge JSON arrays SHALL contain identical data elements (order-independent comparison for elements within arrays)

#### Scenario: HTML export with --no-viz flag produces no file
- **WHEN** the `--no-viz` flag is set
- **THEN** no HTML file SHALL be written, and no memory SHALL be allocated for HTML rendering

### Requirement: Stream Cypher export to file without full in-memory string

The Cypher file export (`exportCypher` for Neo4j, `exportMemgraphCypher` for Memgraph) SHALL write statements to a file Handle incrementally. The export SHALL NOT build the entire Cypher script as a single `Text` value in memory. Each CREATE statement SHALL be written to the Handle as it's generated.

#### Scenario: Cypher export on 100k-node graph completes without OOM
- **WHEN** Graphos exports a Cypher file for a graph with 100,000 nodes
- **THEN** the export SHALL complete without OOM, and peak memory during Cypher export SHALL NOT exceed 1.5× the in-memory Graph size

#### Scenario: Streaming Cypher produces identical statements
- **WHEN** comparing the streaming Cypher output to the previous batch Cypher output on the same codebase
- **THEN** the Cypher files SHALL contain identical CREATE statements (order-independent comparison for node statements)