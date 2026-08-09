## Why

Graphos produces graphs that its own visualizer cannot render. The current `graph.html` (PRD §12) embeds the full graph JSON inline and hands it to vis-network, which freezes the browser above ~10K nodes. The real target — fluid, explorable codebases like the 78K-node solario run — is unreachable today. Worse, the Leiden community detection (PRD §5) already computes 8,519 communities for that run, but `nodeCommunityId` is never written back onto `Node` records, so the HTML viewer renders a flat, uncolored graph with no level-of-detail structure to aggregate on. This change fixes the data join and rebuilds the HTML viewer around community-based level-of-detail (LOD) rendering so 78K-node graphs are fluid and explorable via `graphos serve` (PRD §13.1).

## What Changes

- **BREAKING**: `graph.html` no longer renders every node at once. It renders one dot per community at overview and expands a community's members on drill-down. The vis-network single-flat-graph model is replaced by a two-phase LOD viewer.
- **BREAKING**: `graph.json` `nodes[].community_id` will be populated from the `communities` map during export (currently always `null`). Consumers reading `community_id` directly will see real values instead of `null`.
- Add a community-join pass to the export pipeline: after Leiden detection (PRD §5.1), write `nodeCommunityId` back onto each `Node` record before JSON/HTML export. Pure function, no new IO.
- Add a community-aggregate dataset to `graph.json`: one entry per community with `id`, `member_count`, `cohesion`, `bridge_count`, `color`, `label`, `representative_labels`, and `inter_community_edges`. This is the data the LOD overview renders.
- Rewrite `Infrastructure/Export/HTML.hs` to emit a two-phase viewer: overview (community dots positioned by inter-community edges) → drill-down (expand a community into its member nodes with internal + bridge edges). Served via the existing `graphos serve` static HTTP server (PRD §13.1).
- Switch the vis-network physics solver from `barnesHut` to `forceAtlas2Based` and enable `hideEdgesOnDrag` for the overview phase (matches the graphify HTML approach that handles ~7K nodes acceptably).
- Keep `graph.html` self-contained (inline JSON) — the LOD approach means the HTML carries the full graph for drill-down but the renderer only ever holds ~8K community dots or ~50 member nodes in memory at a time.

## Capabilities

### New Capabilities

- `html-lod-viewer`: Two-phase level-of-detail HTML graph viewer — community-dot overview with click/zoom drill-down into member nodes, served over HTTP via `graphos serve`. Replaces the flat vis-network single-render model.

### Modified Capabilities

- `node-schema`: `nodeCommunityId` SHALL be populated from the Leiden `CommunityMap` during export instead of remaining `Nothing`. The field already exists (PRD §4, node-schema spec) but is never written.

## Impact

**Code**:
- `src/Graphos/Domain/Analysis.hs` — add community-join function (pure, maps `CommunityMap` → updates `Node` records).
- `src/Graphos/UseCase/Export.hs` or `src/Graphos/UseCase/Pipeline.hs` — invoke the join pass after `clusterGraph` and before JSON/HTML export.
- `src/Graphos/Infrastructure/Export/JSON.hs` — serialize the new community-aggregate dataset alongside existing `communities`/`cohesion`/`god_nodes`.
- `src/Graphos/Infrastructure/Export/HTML.hs` — full rewrite of the embedded JS viewer (two-phase LOD, forceAtlas2Based, community drill-down). The streaming-to-handle approach is preserved.
- `src/Graphos/Infrastructure/Export/CommunityGraph.hs` — likely reused/extended for the aggregate dataset (already produces community-level graphs).

**APIs/Dependencies**: No new Haskell dependencies. vis-network CDN dependency remains (no switch to sigma.js in this iteration — Fork B from exploration). The `graph.json` shape gains a top-level `community_aggregates` key; existing consumers that ignore unknown keys are unaffected.

**Systems**: `graphos serve` (Static.hs) unchanged — still serves the output directory over HTTP. The 72MB→ payload is reduced in working-set (renderer memory) but the file size on disk is comparable since drill-down data is embedded.

**Tests**: New Hspec tests for the community-join purity and the aggregate dataset shape. HTML viewer behavior is not unit-tested (canvas rendering) but the JSON it consumes is.

## PDCA Cycle

- **Plan**: Hypothesis — a two-phase LOD viewer backed by correctly-joined community data renders 78K-node graphs fluidly (interactive frame rate, < 3s initial load) where the current viewer freezes. Success measured against PRD §16.1 (100K-node scale target) and PRD §16.2 (large codebase approach): initial load < 3s, interaction latency < 100ms, browser memory < 1GB at 78K nodes. Verified on the solario 78K-node graph as the benchmark.
- **Do**: Implement the community-join pass, add the community-aggregate dataset to `graph.json`, rewrite the HTML viewer as a two-phase LOD renderer with `forceAtlas2Based` + `hideEdgesOnDrag`. Served via `graphos serve`.
- **Check**: Run `graphos <solario-path>` to produce a 78K-node `graph.html`, serve it, and verify: (1) initial load < 3s, (2) community overview renders 8K dots fluidly, (3) drill-down into a community renders its members in < 500ms, (4) browser memory stays < 1GB, (5) `graph.json` `nodes[].community_id` is non-null for community members. Compare against the current freeze-on-load baseline.
- **Act**: If the LOD viewer handles 78K fluidly, standardize the two-phase pattern for future formats (Obsidian, SVG). If it strains at 8K community dots, the next PDCA cycle evaluates sigma.js (WebGL) as Fork A from exploration. If community-join exposes downstream consumers that break on non-null `community_id`, document migration in the node-schema spec.