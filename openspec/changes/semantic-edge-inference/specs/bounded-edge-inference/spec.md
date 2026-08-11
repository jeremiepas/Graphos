# bounded-edge-inference

Delta — new `maxSemanticFanOut` cap for semantic edge inference.

## MODIFIED Requirements

### Requirement: Semantic inference fan-out cap

The system SHALL define `maxSemanticFanOut :: Int` (default 50) bounding the number of
`CodeFile` nodes a single `DocFile` node may match via cosine similarity. Doc nodes whose
top-k similar code nodes exceed the cap SHALL only emit edges for the top-`maxSemanticFanOut`
by similarity score. This extends the bounded-edge-inference family alongside the existing
`maxCommunityBridges` (10000) and `maxLabelFanOut` (20) caps.

#### Scenario: Cap respected on high-fan-out doc node
- **WHEN** a doc node has cosine similarity > threshold with 80 code nodes and
  `maxSemanticFanOut = 50`
- **THEN** only the top-50 code nodes by similarity receive `References` edges

#### Scenario: Cap configurable
- **WHEN** `graphos.yaml` sets `semantic_edges.max_fan_out: 100`
- **THEN** `maxSemanticFanOut` is 100 for that run