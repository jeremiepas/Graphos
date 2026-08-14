## Why

The `graph.html` viewer only offers two depths: a community-dot overview and a single-community drill-down (PRD §12, `refactor-html-large-graph-lod`). Users cannot zoom between abstraction levels — there is no way to see the full graph at the node level, no way to keep multiple communities expanded for cross-community work, and no way to navigate the hierarchy beyond the two fixed phases. A **profondeur (depth) view selector** lets users pick the abstraction level explicitly, making the explorer useful for both architectural overviews and detailed node-level navigation on small-to-mid graphs (the 158K-node case already handled by LOD; the selector adds ergonomics for the common < 10K-node case).

## What Changes

- Add a **depth selector** control to the `graph.html` header (next to the search box) with discrete levels: **Overview** (community dots), **Community** (one community expanded — current drill-down), **Full** (all nodes rendered, no community aggregation), and **Custom** (user-defined N-hop neighborhood around a selected node).
- Refactor the viewer's two-phase state machine (`currentPhase: 'overview' | 'drilldown'`) into a multi-level depth state (`currentDepth: 'overview' | 'community' | 'full' | 'neighborhood'`) with the selector driving transitions.
- Add a **neighborhood depth** sub-control (visible only in Custom mode): an integer input (1–6 hops) that uses BFS expansion from a selected node, reusing the existing `bfsFrom` adjacency already shipped in `graph.json` edges.
- Persist the selected depth in `sessionStorage` so reloads preserve the user's view preference.
- **BREAKING (HTML only)**: the `btnBack` "← Back" button is replaced by the depth selector; the back affordance becomes "select Overview" in the dropdown. The `expandCommunity`/`backToOverview` JS functions are kept as internal helpers but no longer the only entry points.
- No change to `graph.json` shape, `graphos serve`, or any backend stage — this is a viewer-only change confined to `Infrastructure/Export/HTML.hs`.

## Capabilities

### New Capabilities
- `html-depth-selector`: A depth-level selector control in the `graph.html` viewer that switches between abstraction levels (overview, community, full graph, custom neighborhood), with the selected level persisted across reloads via `sessionStorage`.

### Modified Capabilities
- `html-lod-viewer`: The two-phase overview→drill-down state machine becomes one of several selectable depth levels. The drill-down phase is relabeled "Community" depth; the overview phase is unchanged. New "Full" and "Custom (neighborhood)" depth levels are added as peer modes.

## Impact

**Code**:
- `src/Graphos/Infrastructure/Export/HTML.hs` — the only touched file. Adds the selector markup to `htmlHeader`, the depth-state + neighborhood-BFS logic to `htmlBody` (JS), and CSS for the selector. The Haskell streaming JSON embedding is unchanged.
- No Domain/UseCase changes (BFS happens client-side over already-shipped edges; no new data needed from the pipeline).

**APIs/Dependencies**: No new Haskell or JS dependencies. vis-network CDN unchanged. The change is pure HTML/CSS/JS embedded in the Haskell string templates.

**Systems**: `graphos serve` (Static.hs) unchanged. The HTML file remains self-contained. No `graph.json` schema change; existing consumers are unaffected.

**Tests**: HTML viewer behavior is canvas-rendered and not unit-tested (consistent with the existing `refactor-html-large-graph-lod` decision). Manual verification against the criteria in `Check`. No new Hspec tests.

## PDCA Cycle

- **Plan**: Hypothesis — a depth selector makes the `graph.html` explorer usable across abstraction levels on the common < 10K-node graphs (the 158K case is already served by the LOD default). Success measured against PRD §16.1 (interaction latency < 100ms) and PRD §16.2 (large-codebase approach: lower resolution for overview, full detail on demand). Criteria: (1) selector renders in the header on load; (2) switching Overview→Full on a < 1K-node graph renders all nodes within 1s; (3) Custom neighborhood with N=2 around a selected node returns the BFS-induced subgraph matching `graphos neighbors <id> --depth 2`; (4) depth persists across reload via `sessionStorage`; (5) no regression on the 158K-node LOD path (Overview stays the default and the heavy path).
- **Do**: Add the selector markup + CSS to `htmlHeader`; refactor `htmlBody` JS from a two-phase state machine to a multi-depth dispatcher; add a `buildNeighborhoodData(nodeId, hops)` JS function using BFS over `allEdges`; wire `sessionStorage` persistence.
- **Check**: Run `cabal run graphos -- example/` to produce `graph.html`, serve via `graphos serve --dir graphos-out --port 8080`, and verify each depth level: (1) selector present and defaults to Overview; (2) Full depth renders all nodes on a small graph without freezing; (3) Custom depth with N=2 around a known node returns the same node set as the CLI `graphos neighbors <id> --depth 2`; (4) reload preserves the selected depth; (5) the 158K-node path still defaults to Overview and does not regress (no auto-load into Full). `cabal build` zero warnings, `cabal test` green.
- **Act**: If Full depth is usable up to ~5K nodes in the browser, document the threshold in the legend tooltip ("Full view recommended for < 5K nodes"). If the neighborhood BFS is slow in JS above N=4, cap the selector input at N=4 and add a warning. If the depth selector pattern proves ergonomic, standardize it for future formats (Obsidian vault explorer, SVG export level-of-detail).