# Task 3 — Implement Full depth render — PLAN

**Task slug**: `03-implement-full-depth-render`
**Attempt**: 1
**Status**: pending

## Summary

Implement `renderFull()` in `htmlBody` that renders all `allNodes` as individual dots (one per node, colored by community, sized 12, with labels) and all `allEdges` as edges, using a `barnesHut` physics solver that stabilizes then disables.

## Detail

### Scope

- File: `src/Graphos/Infrastructure/Export/HTML.hs`
- Changes to `htmlBody`: implement `function renderFull()` that: (a) calls `destroyCurrentNetwork()` first; (b) builds a `vis.DataSet` from `allNodes` preserving `community_id`, `color`, and `label`; (c) builds an edge `vis.DataSet` from `allEdges`; (d) creates a new `vis.Network` with `barnesHut` options (stabilizes then disables physics); (e) wires `click` event to `showNodeDetail`.

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied:**
- `html-depth-selector/spec.md` — "Switching to Full re-renders all nodes": `switchDepth('full')` renders one dot per individual node on a graph with 942 nodes and 3 communities
- `html-depth-selector/spec.md` — "No overlapping canvases on depth switch": `renderFull()` calls `destroyCurrentNetwork()` which removes the previous canvas
- `html-lod-viewer/spec.md` — "Full depth renders all nodes on explicit selection": renders one dot per individual node (942 dots) with all inter-node edges
- `html-lod-viewer/spec.md` — "No simultaneous full-graph render unless explicitly selected": Full is only rendered when `switchDepth('full')` is called

**PASS conditions:**
1. `switchDepth('full')` on the example graph (< 1K nodes) renders one dot per node within 1s (manual verification via browser)
2. The rendered node count equals `allNodes.length` (verified via devtools: `vis.Network` body has N node entries matching `allNodes.length`)
3. After switching to Full depth, at most one `<canvas>` exists inside `#graph` (previous canvas destroyed)
4. `cabal build` exits with zero warnings
5. `cabal test` exits with code 0

**FAIL boundaries:**
- FAIL if Full depth takes more than 5s to render on the example graph (indicates an inefficient render path)
- FAIL if rendered node count does not match `allNodes.length` (off-by-one or filtering bug)
- FAIL if multiple `<canvas>` elements exist inside `#graph` after switch (destroyCurrentNetwork didn't clean up)
- FAIL if `cabal build` produces any warnings
- FAIL if `cabal test` exits non-zero

### Affected modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — `htmlBody` function (embedded JavaScript, `renderFull` implementation)

### Prerequisites

- Task 2 (multi-depth dispatcher) is complete — `switchDepth('full')` routes to `renderFull()`
- `allNodes` and `allEdges` data arrays are available in the `htmlBody` scope

### Risks

- **Performance on large graphs**: Rendering all nodes with vis-network barnesHut physics on graphs > 5K nodes may freeze the browser. Mitigation: add a legend tooltip "Full view recommended for < 5K nodes" (referenced in design Decision 5). The default is always Overview so large graphs never auto-load Full.
- **Physics stabilization time**: `barnesHut` solver needs time to stabilize. If `forceSimulation.stabilization` is not configured correctly, the network may appear jumbled. Mitigation: set `physics.stabilization: {iterations: 100, exitOnStabilization: true}` or equivalent vis-network options.
- **Edge rendering on large graphs**: On dense graphs, rendering all edges simultaneously may be slow. Mitigation: vis-network handles edge rendering efficiently with its internal data structures; no special optimization needed for < 5K nodes.

## Result

<!-- Pending implementation -->
