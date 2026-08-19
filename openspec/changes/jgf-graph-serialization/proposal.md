## Why

`graphos-out/graph.json` uses an **ad-hoc, unversioned** schema: a top-level
object with `nodes`, `edges`, `communities`, `cohesion`, `god_nodes`,
`community_labels` (see `Infrastructure/Export/JSON.hs`). There is no format
identifier, no schema version, and no interoperability with the wider graph
tooling ecosystem — every consumer hard-codes graphos's private shape, and a
future field change is a silent breaking change with no version to gate on.

A published standard already fits: **JSON Graph Format (JGF)** — media type
`application/vnd.jgf+json`, spec at `jsongraphformat.info` — represents a graph
as `{ "graph": { directed, type, label, metadata, nodes, edges } }`, where a node
has an `id` + `label` + `metadata`, and an edge has `source` / `relation` /
`target` + `metadata`. graphos's current node/edge shape maps onto JGF almost
1:1, so adopting it is low-friction and makes graph files **self-describing,
versioned, and portable** to any JGF reader.

## What Changes

- Serialize the graph as a **JGF document** (`application/vnd.jgf+json`): wrap
  nodes/edges in the JGF `graph` envelope, set `directed: true` and
  `type: "graphos.code-knowledge-graph"`, and carry graphos-specific fields under
  `metadata`.
- Map fields losslessly: node `id`/`label` stay top-level; `source_file`, `kind`,
  `community`, `line_start`/`line_end`, `signature`, `is_bridge`, `degree` move to
  node `metadata`. Edge `source`/`target`/`relation` stay top-level; `id`,
  `weight`, `confidence`, `extra` move to edge `metadata`. Graph-level
  `communities`, `cohesion`, `god_nodes`, `community_labels`, `graph_hash`, and a
  `schemaVersion` live under `graph.metadata.graphos`.
- Reader accepts **both** the new JGF envelope and the legacy top-level
  `nodes`/`edges` schema (detected by presence of the top-level `graph` key), so
  existing `graph.json` files keep loading during the transition.
- Apply the same envelope to the **checkpoint** writer; JSON stays the on-disk
  format (no binary format in this change).

## Capabilities

### New Capabilities
- `jgf-serialization`: the canonical graph file is a versioned JSON Graph Format
  document, with graphos-specific data under standard `metadata` fields.

### Modified Capabilities
<!-- Full-pipeline output and the graph loader change format; node/edge content
     is preserved (lossless round-trip). -->

## Impact

- **Writer:** `Infrastructure/Export/JSON.hs` (graph + checkpoint) emits the JGF
  envelope.
- **Reader:** `loadGraphFromFile` parses JGF and legacy schema; all consumers
  (MCP, query, HTML export) that go through it inherit compatibility.
- **Interop:** files become valid JGF (`application/vnd.jgf+json`), readable by
  standard tooling; the format is versioned via `graph.metadata.graphos.schemaVersion`.
- External consumers reading the raw private schema must switch to JGF; mitigated
  by reader back-compat and a documented format in the changelog.
