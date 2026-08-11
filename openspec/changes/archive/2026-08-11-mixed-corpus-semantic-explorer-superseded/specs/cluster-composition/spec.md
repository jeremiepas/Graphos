# cluster-composition

Post-clustering computation of per-community `CommunityComposition` (code/doc/other counts,
dominant kind, mixed ratio, cross-type edge count), persisted to `graph.json` and surfaced in
the HTML viewer as a cluster badge.

## ADDED Requirements

### Requirement: CommunityComposition record

The system SHALL define a `CommunityComposition` record with fields:
`ccCodeCount :: Int`, `ccDocCount :: Int`, `ccOtherCount :: Int`,
`ccDominantKind :: Maybe Text`, `ccMixedRatio :: Double`, `ccCodeDocEdges :: Int`.
`ccMixedRatio` SHALL be `min(code,doc) / max(code,doc)` (0 when max is 0, 1 when balanced).
The record SHALL have `ToJSON` and `FromJSON` instances.

#### Scenario: Pure-code community
- **WHEN** a community has 10 `CodeFile` nodes and 0 `DocFile` nodes
- **THEN** `ccCodeCount = 10`, `ccDocCount = 0`, `ccMixedRatio = 0.0`

#### Scenario: Balanced mixed community
- **WHEN** a community has 8 `CodeFile` nodes and 8 `DocFile` nodes
- **THEN** `ccMixedRatio = 1.0`

### Requirement: Compositions computed post-clustering

The pipeline SHALL compute `Map CommunityId CommunityComposition` for every community after
Leiden runs, counting members by `FileType`, deriving `ccDominantKind` from the most frequent
`nodeKind` among members, and counting `References` edges inside the community that cross
`CodeFile`↔`DocFile`. The result SHALL be persisted to `graph.json` under a `compositions` key.

#### Scenario: Composition counts match membership
- **WHEN** community 483 has 12 code + 4 doc + 0 other members and 3 `References` edges
  crossing code↔doc
- **THEN** the persisted composition for 483 is
  `{ "code": 12, "doc": 4, "other": 0, "dominant_kind": "function", "mixed_ratio": 0.33, "code_doc_edges": 3 }`

#### Scenario: Legacy graph loads without compositions
- **WHEN** `graph.json` has no `compositions` key
- **THEN** the loader returns an empty compositions map and queries succeed without error

### Requirement: HTML viewer renders composition badge

The `graph.html` viewer SHALL render a composition badge on every community dot (overview
mode) and on every community drill-down header. The badge SHALL display code count, doc
count, and cross-type edge count in a compact form (e.g. `🔧 12 / 📄 4 / 🌉 3`). The badge
SHALL be visible at all depth levels that show communities.

#### Scenario: Badge on community dot
- **WHEN** the viewer renders community 483 in overview mode
- **THEN** the dot's tooltip or adjacent label shows `🔧 12 / 📄 4 / 🌉 3`

#### Scenario: Badge on drill-down
- **WHEN** the user drills into community 483
- **THEN** the community header shows the same composition badge

### Requirement: Composition-aware labeling prompt

The `labelPrompt` function SHALL tag each top node with `(code)` or `(doc)` based on its
`FileType` and SHALL include a composition summary line in the prompt. The prompt preamble
SHALL frame the task as mixed code-and-knowledge and SHALL instruct the LLM to name the
concept that unifies the community, not the most frequent word.

#### Scenario: Mixed cluster prompt includes composition
- **WHEN** `labelPrompt` is called for community 483 with 12 code + 4 doc members
- **THEN** the prompt contains a line like
  `"Community 483 (cohesion: 0.72, size: 16, composition: 12 code + 4 docs, 3 code↔doc links):"`
  and the top nodes are split into "Top code nodes:" and "Top doc nodes:" lines

#### Scenario: Pure-code cluster prompt still works
- **WHEN** `labelPrompt` is called for a community with only `CodeFile` nodes
- **THEN** the prompt shows "Top code nodes:" and no "Top doc nodes:" line; the composition
  line reads `"composition: 10 code + 0 docs"`