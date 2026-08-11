# cluster-composition

Post-clustering computation of per-community `CommunityComposition` (code/doc/other counts,
dominant kind, mixed ratio, cross-type edge count), persisted to `graph.json` and surfaced
in the HTML viewer as a cluster badge.

## ADDED Requirements

### Requirement: CommunityComposition record

The system SHALL define a `CommunityComposition` record with fields:
`ccCodeCount :: Int`, `ccDocCount :: Int`, `ccOtherCount :: Int`,
`ccDominantKind :: Maybe Text`, `ccMixedRatio :: Double`, `ccCodeDocEdges :: Int`.
`ccMixedRatio` SHALL be `min(code,doc) / max(code,doc)` (0 when max is 0, 1 when balanced).
`ccDocCount` SHALL count `DocFile`, `PaperFile`, and `OfficeFile` nodes together (doc-like
corpora). `ccOtherCount` SHALL count `ImageFile`, `VideoFile`, `AudioFile`. The record SHALL
have `ToJSON` and `FromJSON` instances.

#### Scenario: Pure-code community
- **WHEN** a community has 10 `CodeFile` nodes and 0 doc-like nodes
- **THEN** `ccCodeCount = 10`, `ccDocCount = 0`, `ccOtherCount = 0`, `ccMixedRatio = 0.0`

#### Scenario: Balanced mixed community
- **WHEN** a community has 8 `CodeFile` nodes and 8 `DocFile` nodes
- **THEN** `ccMixedRatio = 1.0`

#### Scenario: Paper counted as doc
- **WHEN** a community has 6 `CodeFile` nodes and 3 `PaperFile` nodes
- **THEN** `ccDocCount = 3` (paper counts as doc-like), `ccMixedRatio = 0.5`

### Requirement: Compositions computed post-clustering

The pipeline SHALL compute `Map CommunityId CommunityComposition` for every community after
Leiden runs, counting members by `FileType`, deriving `ccDominantKind` from the most frequent
`nodeKind` among members (ignoring `Nothing` kinds), and counting `References` edges inside
the community that cross `CodeFile`↔doc-like (`DocFile`/`PaperFile`/`OfficeFile`). The result
SHALL be persisted to `graph.json` under a `compositions` key.

#### Scenario: Composition counts match membership
- **WHEN** community 483 has 12 code + 4 doc + 0 other members and 3 `References` edges
  crossing code↔doc inside the community
- **THEN** the persisted composition for 483 is
  `{ "code": 12, "doc": 4, "other": 0, "dominant_kind": "function", "mixed_ratio": 0.33, "code_doc_edges": 3 }`

#### Scenario: Cross-type edge count excludes non-references edges
- **WHEN** community 483 has 3 `References` edges crossing code↔doc and 5 `contains` edges
  crossing code↔doc
- **THEN** `ccCodeDocEdges = 3` (only `References` edges counted)

#### Scenario: Dominant kind ignores Nothing
- **WHEN** a community has 5 nodes with `nodeKind = Just "function"`, 3 with
  `nodeKind = Nothing`, and 2 with `nodeKind = Just "module"`
- **THEN** `ccDominantKind = Just "function"` (most frequent non-Nothing kind)

#### Scenario: Legacy graph loads without compositions
- **WHEN** `graph.json` has no `compositions` key
- **THEN** the loader returns an empty compositions map and queries succeed without error

### Requirement: HTML viewer renders composition badge

The `graph.html` viewer SHALL render a composition badge on every community dot (overview
mode) and on every community drill-down header. The badge SHALL display code count, doc
count, and cross-type edge count in a compact form (e.g. `🔧 12 / 📄 4 / 🌉 3`). The badge
SHALL be visible at all depth levels that show communities. When compositions are absent
(legacy graph), the badge SHALL be omitted gracefully (no error, no placeholder).

#### Scenario: Badge on community dot
- **WHEN** the viewer renders community 483 in overview mode and compositions are present
- **THEN** the dot's tooltip shows `🔧 12 / 📄 4 / 🌉 3`

#### Scenario: Badge on drill-down
- **WHEN** the user drills into community 483
- **THEN** the community header shows the composition badge next to the label

#### Scenario: Legacy graph omits badge
- **WHEN** the viewer renders a graph without `compositions` in the embedded JSON
- **THEN** no badge is rendered and no error is logged