# Bounded Edge Inference Capability

## ADDED Requirements

### Requirement: Community bridges derive from real adjacency

`inferCommunityBridges` (PRD §3 infer stage) MUST emit centroid-to-centroid inferred edges only for community pairs connected by at least one real inter-community edge, and the result MUST be bounded by a named cap constant. It SHALL NOT enumerate all community pairs.

#### Scenario: Adjacent communities are bridged

- **WHEN** two communities are connected by at least one real edge between their members
- **THEN** exactly one inferred centroid bridge is emitted for that pair, verifiable by `cabal test`

#### Scenario: Disconnected communities are not bridged

- **WHEN** two communities share no real inter-community edge
- **THEN** no inferred bridge edge is emitted between them

#### Scenario: Bridge count does not scale quadratically

- **WHEN** a graph produces C communities with far fewer than C² inter-community adjacencies
- **THEN** the number of inferred bridges is at most the number of adjacent community pairs, capped by the named constant

### Requirement: Log-linear edge deduplication

Edge and candidate deduplication in inference and analysis (PRD §16.2 scalability) MUST run in O(k log k), preserving input order and first-occurrence-wins semantics. `nubBy` SHALL NOT be used on lists that scale with graph size.

#### Scenario: Dedup matches nubBy semantics

- **WHEN** a list containing duplicate (source, target) edges in a given order is deduplicated
- **THEN** the output equals the reference `nubBy` result (first occurrence kept, order preserved), verifiable by `cabal test` including a QuickCheck property

### Requirement: Capped doc-code label fan-out

`inferCodeDocEdges` MUST skip labels whose code-node match count exceeds a named fan-out cap; unambiguous labels (matches within the cap) SHALL continue to produce reference edges.

#### Scenario: Ambiguous labels produce no edges

- **WHEN** a doc node's label matches more code nodes than the fan-out cap
- **THEN** no doc-code edges are emitted for that label

#### Scenario: Specific labels still link

- **WHEN** a doc node's label matches a number of code nodes within the cap
- **THEN** reference edges are emitted for those matches, with no duplicate (source, target) pairs

### Requirement: Inference completes within memory bounds at scale

The infer stage MUST complete on 75k-node / 80k-edge graphs without multi-gigabyte memory growth (PRD §16.1, §16.2); inferred edge counts SHALL be proportional to real inter-community adjacency rather than the square of the community count.

#### Scenario: Repository-scale inference stays bounded

- **WHEN** the full pipeline runs at Normal edge density on this repository
- **THEN** the "Inferred N additional edges" count is on the order of the graph's real inter-community adjacency (thousands), not ~C²/2 (previously 48,112 for 314 communities)
