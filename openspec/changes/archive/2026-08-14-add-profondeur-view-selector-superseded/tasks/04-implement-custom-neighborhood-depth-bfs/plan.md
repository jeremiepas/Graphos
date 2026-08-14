# Task 4 — Implement Custom (neighborhood) depth with client-side BFS — PLAN

**Task slug**: `04-implement-custom-neighborhood-depth-bfs`
**Attempt**: 1
**Status**: pending

## Summary

Implement `renderCustom(nodeId, hops)` and `buildNeighborhoodData(nodeId, hops)` in `htmlBody`. Build a `nodeAdj` map (nodeId → [neighborIds]) once on load from `allEdges`. `buildNeighborhoodData` performs BFS to `hops` levels, returns the induced subgraph. The neighborhood input `#neighborhoodHops` is shown when `Custom` is selected; clicking a node in Custom depth re-runs BFS.

## Detail

### Scope

- File: `src/Graphos/Infrastructure/Export/HTML.hs`
- Changes to `htmlBody`: (a) after `allEdges` is loaded, build `const nodeAdj = {}` with bidirectional adjacency; (b) implement `function buildNeighborhoodData(nodeId, hops)` using visited Set + frontier BFS; (c) implement `function renderCustom(nodeId, hops)` calling `destroyCurrentNetwork()`, building `vis.DataSet` from induced nodes/edges, rendering with barnesHut; (d) wire `#depthSelector` change handler to toggle `.active` on `#neighborhoodHops` when `custom` is selected; (e) wire network `click` in Custom depth to call `renderCustom(clickedNodeId, currentHops)`; (f) wire `#neighborhoodHops` `change` to re-run `renderCustom` with current focus node and new hop count.

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied:**
- `html-depth-selector/spec.md` — "Custom depth shows neighborhood input": `#neighborhoodHops` becomes visible when Custom is selected
- `html-depth-selector/spec.md` — "N=2 neighborhood matches CLI neighbors": clicked node X with hops 2 renders a node set matching `graphos neighbors X --depth 2`
- `html-depth-selector/spec.md` — "Changing hop count re-runs BFS": changing from 2 to 3 re-runs BFS and re-renders

**PASS conditions:**
1. Selecting `Custom` makes `#neighborhoodHops` visible with value 2, `min=1`, `max=6`
2. Clicking a known node `X` with hops 2 renders a node set equal to `cabal run graphos -- neighbors X --depth 2` output (node count matches; spot-check 3 node ids match)
3. Changing the input to 3 and re-clicking `X` renders a node set that is a superset of the N=2 set
4. `cabal build` exits with zero warnings
5. `cabal test` exits with code 0

**FAIL boundaries:**
- FAIL if `#neighborhoodHops` is visible when Custom is NOT selected (should be hidden)
- FAIL if the N=2 BFS node count does not match the CLI output (BFS or adjacency bug)
- FAIL if changing hop count does not re-render the neighborhood (missing event wire)
- FAIL if the N=3 set is NOT a superset of the N=2 set (BFS traversal bug)
- FAIL if `cabal build` produces any warnings
- FAIL if `cabal test` exits non-zero

### Affected modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — `htmlBody` function (embedded JavaScript, adjacency map, BFS, renderCustom, event wiring)

### Prerequisites

- Task 2 (multi-depth dispatcher) is complete — `switchDepth('custom')` routes to `renderCustom()`
- Task 1 (markup + CSS) is complete — `#neighborhoodHops` input exists in the DOM
- `allEdges` data array is available in the `htmlBody` scope

### Risks

- **Edge direction**: `allEdges` may have directed edges (from → to), but BFS neighborhood should treat them as undirected (bidirectional). Mitigation: add both directions to `nodeAdj`: `(nodeAdj[e.from] || []).push(e.to); (nodeAdj[e.to] || []).push(e.from);`
- **BFS performance on dense graphs**: BFS on dense graphs with large hop counts may be slow. The input is capped at `max=6`, but even N=4 on a dense graph could produce thousands of nodes. Mitigation: add a tooltip warning if the induced subgraph exceeds 2K nodes (referenced in design Risks).
- **Stale node references**: If `sessionStorage` restores a node id that no longer exists in the current `allNodes` (e.g., filtered data), the BFS would produce an empty result. Mitigation: check `nodeAdj[nodeId]` is defined before running BFS; fall back to Overview if not.
- **CLI mismatch**: The `graphos neighbors` CLI command uses the server-side adjacency from the pipeline. Client-side BFS must produce the exact same result. Mitigation: run `cabal run graphos -- neighbors X --depth 2` and diff the output against the browser-rendered node set.

## Result

<!-- Pending implementation -->
