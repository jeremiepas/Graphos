## MODIFIED Requirements

### Requirement: Canonical Node field set

The `Node` type SHALL contain exactly the 13 canonical fields (`nodeId`, `nodeLabel`, `nodeFileType`, `nodeSourceFile`, `nodeSource`, `nodeLineStart`, `nodeLineEnd`, `nodeSignature`, `nodeCommunityId`, `nodeKind`, `nodeDegree`, `nodeIsBridge`, `nodeExtra`) and SHALL NOT define the legacy fields `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`. The `nodeSource` field SHALL hold the configured source name a node was detected under, or `Nothing` for single-path runs without `sources:` configured. The `nodeCommunityId` field SHALL be populated from the Leiden `CommunityMap` (PRD §5.1) before JSON and HTML export — it SHALL NOT remain `Nothing` for any node that appears in a detected community.

#### Scenario: Legacy fields absent from the type

- **WHEN** the codebase is searched for `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`
- **THEN** no references exist in `src/`, `app/`, or `tests/`

#### Scenario: Node JSON omits legacy keys

- **WHEN** any `Node` is serialized to JSON
- **THEN** the output contains no `source_location`, `source_url`, `captured_at`, `author`, or `contributor` keys

#### Scenario: Multi-source node carries source name

- **WHEN** a node detected under configured source `repoA` is serialized to JSON
- **THEN** its `source` key equals `"repoA"`

#### Scenario: Single-path node serializes null source

- **WHEN** a node from a run without `sources:` configured is serialized to JSON
- **THEN** its `source` key is `null`

#### Scenario: Community ID populated after Leiden

- **WHEN** the pipeline runs Leiden community detection producing a `CommunityMap` that assigns node `n1` to community `4`, and the pipeline then exports `graph.json`
- **THEN** node `n1`'s `community_id` field in `graph.json` is `4` (not `null`)

#### Scenario: Every community member has a non-null community_id

- **WHEN** a graph with 78,529 nodes and 8,519 communities is exported after the community-join pass
- **THEN** every one of the 78,529 nodes has a non-null `community_id` matching its assigned community

#### Scenario: Nodes outside any community remain null

- **WHEN** a node is not present in any community of the `CommunityMap` (e.g., an isolated node)
- **THEN** its `community_id` remains `null` (the join pass does not fabricate a community)

### Requirement: Cache tolerance for legacy keys

Loading cached extractions produced before this change SHALL succeed; legacy JSON keys are ignored and not carried forward. Cached extractions whose nodes lack a `source` key SHALL load with `nodeSource = Nothing`.

#### Scenario: Old cache file parses

- **WHEN** a cached extraction containing `captured_at` or `source_location` keys is loaded
- **THEN** parsing succeeds and the resulting nodes contain no legacy data outside `nodeExtra`

#### Scenario: Pre-change cache loads without source

- **WHEN** a cached extraction written before the `nodeSource` field existed is loaded
- **THEN** parsing succeeds and every resulting node has `nodeSource = Nothing`