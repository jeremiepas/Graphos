# Node Schema Capability

## Purpose

Define the canonical schema for graph nodes, ensuring a stable, minimal field set and migration path for legacy fields.

## Requirements

### Requirement: Canonical Node field set

The `Node` type SHALL contain exactly the 12 canonical fields (`nodeId`, `nodeLabel`, `nodeFileType`, `nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`, `nodeSignature`, `nodeCommunityId`, `nodeKind`, `nodeDegree`, `nodeIsBridge`, `nodeExtra`) and SHALL NOT define the legacy fields `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`.

#### Scenario: Legacy fields absent from the type

- **WHEN** the codebase is searched for `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, or `nodeContributor`
- **THEN** no references exist in `src/`, `app/`, or `tests/`

#### Scenario: Node JSON omits legacy keys

- **WHEN** any `Node` is serialized to JSON
- **THEN** the output contains no `source_location`, `source_url`, `captured_at`, `author`, or `contributor` keys

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
