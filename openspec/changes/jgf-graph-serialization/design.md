## Context

`Infrastructure/Export/JSON.hs` writes `{ nodes, edges, communities, cohesion,
god_nodes, community_labels }` and `loadGraphFromFile` reads it back. The shape
is private, unversioned and undocumented. JSON Graph Format (JGF) is a published
JSON spec for graphs whose node/edge model already matches graphos, so it is the
natural standard to adopt while keeping JSON (the stated preference).

## Goals / Non-Goals

**Goals:**
- A self-describing, **versioned**, standard on-disk graph format.
- Lossless round-trip of every current field.
- Backward-compatible loading of existing `graph.json` files.

**Non-Goals:**
- A binary / memory-mapped format (that is a separate performance change).
- Multi-graph JGF (`graphs` array) or JSON-LD/RDF representations.
- Changing node identity or the graph model.

## Decisions

- **Adopt JGF (`application/vnd.jgf+json`) over GraphSON / JSON-LD.**
  - JGF is the simplest published JSON graph spec and its node/edge shape already
    matches graphos; JSON-native (matches the JSON preference).
  - *GraphSON* — rejected, TinkerPop-specific and more verbose.
  - *JSON-LD / RDF* — rejected, triple model, not a natural fit for a labelled
    property graph and heavier for consumers.
- **`nodes` as a JGF object keyed by node id** (spec-canonical), edges as an
  array of `{source, relation, target, directed, metadata}`.
- **graphos-specific data under `metadata`**, never as ad-hoc top-level keys:
  node fields under node `metadata`; `communities`/`cohesion`/`god_nodes`/
  `community_labels`/`graph_hash`/`schemaVersion` under `graph.metadata.graphos`.
- **Versioned envelope:** `graph.metadata.graphos.schemaVersion` gates future
  changes; readers refuse unknown **major** versions with a clear error.
- **Dual-format reader:** presence of a top-level `graph` object ⇒ JGF; otherwise
  the legacy `nodes`/`edges` schema. Kept for a deprecation window.

## Risks / Trade-offs

- [Node ids are large snippet-bearing strings used as object keys] → unchanged
  from today (ids are already strings); no new constraint. Compacting ids is
  out of scope.
- [External tools reading the private schema break] → reader back-compat + a
  changelog entry + the documented media type; graphos's own consumers go through
  `loadGraphFromFile` and are unaffected.
- [Slight size change from metadata nesting] → negligible relative to snippet
  labels.

## Migration Plan

- Writer emits JGF; reader accepts JGF **and** legacy for a deprecation window.
- Optional `graphos migrate-graph <file>` (or automatic re-write on next full
  run) upgrades legacy files in place.
- Verify with `cabal test`: round-trip (write→read) equality on a fixture graph,
  legacy-file load, and unknown-major-version rejection.
