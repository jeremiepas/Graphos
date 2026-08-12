# Node Schema Capability

## Purpose

Define the canonical schema for graph nodes, ensuring a stable, minimal field set and migration path for legacy fields.
## Requirements
### Requirement: Canonical Node field set

The `Node` type SHALL contain exactly the 12 canonical fields (`nodeId`, `nodeLabel`, `nodeFileType`, `nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`, `nodeSignature`, `nodeCommunityId`, `nodeKind`, `nodeDegree`, `nodeIsBridge`, `nodeExtra`) and SHALL NOT define the legacy fields `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`. The `nodeCommunityId` field SHALL be populated from the Leiden `CommunityMap` (PRD §5.1) before JSON and HTML export — it SHALL NOT remain `Nothing` for any node that appears in a detected community.

#### Scenario: Legacy fields absent from the type

- **WHEN** the codebase is searched for `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`
- **THEN** no references exist in `src/`, `app/`, or `tests/`

#### Scenario: Node JSON omits legacy keys

- **WHEN** any `Node` is serialized to JSON
- **THEN** the output contains no `source_location`, `source_url`, `captured_at`, `author`, or `contributor` keys

#### Scenario: Community ID populated after Leiden

- **WHEN** the pipeline runs Leiden community detection producing a `CommunityMap` that assigns node `n1` to community `4`, and the pipeline then exports `graph.json`
- **THEN** node `n1`'s `community_id` field in `graph.json` is `4` (not `null`)

#### Scenario: Every community member has a non-null community_id

- **WHEN** a graph with 78,529 nodes and 8,519 communities is exported after the community-join pass
- **THEN** every one of the 78,529 nodes has a non-null `community_id` matching its assigned community

#### Scenario: Nodes outside any community remain null

- **WHEN** a node is not present in any community of the `CommunityMap` (e.g., an isolated node)
- **THEN** its `community_id` remains `null` (the join pass does not fabricate a community)

### Requirement: Conversation timestamps migrate to nodeExtra

Conversation-derived nodes SHALL store their capture timestamp under `nodeExtra` (key `capturedAt`) instead of the removed `nodeCapturedAt` field.

#### Scenario: Conversation node carries timestamp in extra

- **WHEN** a conversation is ingested and converted to nodes
- **THEN** each resulting node's `nodeExtra` contains a `capturedAt` value equal to the conversation timestamp

### Requirement: Source location derives from line fields

Consumers that previously read `nodeSourceLocation` (context formatting, Neo4j/Memgraph export) SHALL derive location display from `nodeLineStart`/`nodeLineEnd` when present.

#### Scenario: Context formatting shows line-based location

- **WHEN** a node with `nodeLineStart = Just 10` is formatted for context output
- **THEN** the rendered location suffix is derived from the line fields (e.g., `:10`)

#### Scenario: Graph export unaffected structurally

- **WHEN** the full pipeline runs on a test codebase before and after legacy field removal
- **THEN** `graph.json` node count, edge count, and community count are identical

### Requirement: Cache tolerance for legacy keys

Loading cached extractions produced before this change SHALL succeed; legacy JSON keys are ignored and not carried forward.

#### Scenario: Old cache file parses

- **WHEN** a cached extraction containing `captured_at` or `source_location` keys is loaded
- **THEN** parsing succeeds and the resulting nodes contain no legacy data outside `nodeExtra`

