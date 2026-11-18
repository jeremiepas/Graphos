## ADDED Requirements

### Requirement: Bounded bridge-node table

The "Bridge Nodes (Articulation Points)" section of `GRAPH_REPORT.md` SHALL list at most a named constant number of articulation points (default 50), ordered by degree descending, excluding nodes of degree at most 1 (PRD §12 export formats). Each row SHALL render label, kind, source file and degree rather than the raw node id, and the section SHALL state the total number of articulation points and how many were omitted.

#### Scenario: Large graph report stays readable
- **WHEN** the report is rendered for a graph with 18,894 articulation points
- **THEN** the bridge table has at most 50 rows, a footer states `18,894 articulation points, 18,844 omitted`, and the report file is under 2 MB, verifiable by `cabal test` on the report renderer with a synthetic analysis

#### Scenario: Leaves are not bridges
- **WHEN** an articulation point has degree 1
- **THEN** it does not appear in the bridge table

### Requirement: Edge-drop accounting at build

The build stage SHALL log, at INFO, the number of extracted edges it discarded because an
endpoint was unknown and the number collapsed because they shared a (source, target) key, so the
difference between the extraction's edge count and the exported edge count is explained (PRD
§3.3 build stage, §16.3 reliability).

#### Scenario: Dropped edges are counted
- **WHEN** extraction yields 151,689 edges and the built graph exports 136,340
- **THEN** the log contains one line reporting the unknown-endpoint count and the collapsed-duplicate count whose sum is 15,349, verifiable by `cabal test` on a synthetic extraction with one dangling and one duplicate edge
