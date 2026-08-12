> **⚠ STALE SCALE NUMBERS — revised 2026-08-10.** This proposal was written assuming the solario run was **78K nodes / 8,519 communities / 72 MB HTML**. Measured reality is **158,166 nodes / 184,616 edges / 17,651 communities / 157 MB HTML** — 2× the assumption on every axis. Several claims below are stale as a result; each is marked `[STALE: …]` inline with the corrected figure. The architectural response to the scale revision lives in `design.md` (Decisions 3, 4, 5 REVISED; Decisions 6–9 new): the inline-JSON self-contained HTML is replaced by a small HTML + `graph.sqlite` store queried via `@sqlite.org/sqlite-wasm` + OPFS, the renderer is switched from vis-network to sigma.js v3, and `file://` capability is dropped. This proposal text is preserved as-is for traceability; the authoritative scope is `design.md`.

## Why

Graphos produces graphs that its own visualizer cannot render. The current `graph.html` (PRD §12) embeds the full graph JSON inline and hands it to vis-network, which freezes the browser above ~10K nodes. The real target — fluid, explorable codebases like the 78K-node solario run [STALE: 158,166 nodes] — is unreachable today. Worse, the Leiden community detection (PRD §5) already computes 8,519 communities for that run [STALE: 17,651 communities], but `nodeCommunityId` is never written back onto `Node` records, so the HTML viewer renders a flat, uncolored graph with no level-of-detail structure to aggregate on. This change fixes the data join and rebuilds the HTML viewer around community-based level-of-detail (LOD) rendering so 78K-node graphs [STALE: 158K] are fluid and explorable via `graphos serve` (PRD §13.1). [STALE: the "self-contained inline-JSON HTML" approach is abandoned in `design.md` Decision 3 REVISED — the 157 MB inline payload is the failure mode; member data moves to a `graph.sqlite` store queried via `@sqlite.org/sqlite-wasm` + OPFS, the renderer switches to sigma.js v3, and `file://` is dropped.]

## What Changes

- **BREAKING**: `graph.html` no longer renders every node at once. It renders one dot per community at overview and expands a community's members on drill-down. The vis-network single-flat-graph model is replaced by a two-phase LOD viewer.
- **BREAKING**: `graph.json` `nodes[].community_id` will be populated from the `communities` map during export (currently always `null`). Consumers reading `community_id` directly will see real values instead of `null`.
- Add a community-join pass to the export pipeline: after Leiden detection (PRD §5.1), write `nodeCommunityId` back onto each `Node` record before JSON/HTML export. Pure function, no new IO.
- Add a community-aggregate dataset to `graph.json`: one entry per community with `id`, `member_count`, `cohesion`, `bridge_count`, `color`, `label`, `representative_labels`, and `inter_community_edges`. This is the data the LOD overview renders. [STALE: `inter_community_edges` is a scalar `Int` in the shipped type but the spec requires `[(targetCid, count)]` pairs — fixed in `design.md` Decision 8.]
- Rewrite `Infrastructure/Export/HTML.hs` to emit a two-phase viewer: overview (community dots positioned by inter-community edges) → drill-down (expand a community into its member nodes with internal + bridge edges). Served via the existing `graphos serve` static HTTP server (PRD §13.1). [STALE: the rewrite also drops inline JSON, switches vis-network → sigma.js v3, and queries `graph.sqlite` on drill-down — see `design.md` Decisions 3, 6.]
- Switch the vis-network physics solver from `barnesHut` to `forceAtlas2Based` and enable `hideEdgesOnDrag` for the overview phase (matches the graphify HTML approach that handles ~7K nodes acceptably). [STALE: vis-network is replaced by sigma.js v3 (WebGL); the solver becomes `forceAtlas2` from `graphology-layout-forceatlas2` and `hideEdgesOnDrag` becomes `hideEdgesOnMove` — see `design.md` Decision 6.]
- Keep `graph.html` self-contained (inline JSON) — the LOD approach means the HTML carries the full graph for drill-down but the renderer only ever holds ~8K community dots or ~50 member nodes in memory at a time. [STALE: this bullet is reversed by `design.md` Decision 3 REVISED. The 157 MB inline payload is the failure mode; the HTML is now ~2 MB and member data lives in `graph.sqlite`. `file://` capability is dropped. Browser heap target is < 200 MB, down from the > 1 GB the inline approach costs.]

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

**APIs/Dependencies**: No new Haskell dependencies. vis-network CDN dependency remains (no switch to sigma.js in this iteration — Fork B from exploration). [STALE: a new Haskell dependency on `direct-sqlite` is added for the `graph.sqlite` export (`design.md` Decision 4); vis-network CDN is replaced by sigma.js v3 + graphology + `@sqlite.org/sqlite-wasm` (`design.md` Decisions 3, 6).] The `graph.json` shape gains a top-level `community_aggregates` key; existing consumers that ignore unknown keys are unaffected.

**Systems**: `graphos serve` (Static.hs) unchanged — still serves the output directory over HTTP. The 72MB→ [STALE: 157 MB] payload is reduced in working-set (renderer memory) but the file size on disk is comparable since drill-down data is embedded. [STALE: Static.hs gains COOP/COEP cross-origin isolation headers for OPFS (`design.md` Decision 7); the HTML shrinks to ~2 MB and drill-down data moves to `graph.sqlite` (~66 MB); working-set target drops from < 1 GB to < 200 MB.]

**Tests**: New Hspec tests for the community-join purity and the aggregate dataset shape. HTML viewer behavior is not unit-tested (canvas rendering) but the JSON it consumes is.

## PDCA Cycle

- **Plan**: Hypothesis — a two-phase LOD viewer backed by correctly-joined community data renders 78K-node [STALE: 158K-node] graphs fluidly (interactive frame rate, < 3s initial load) where the current viewer freezes. Success measured against PRD §16.1 (100K-node scale target) and PRD §16.2 (large codebase approach): initial load < 3s, interaction latency < 100ms, browser memory < 1GB [STALE: < 200 MB] at 78K [STALE: 158K] nodes. Verified on the solario 78K-node [STALE: 158K-node] graph as the benchmark.
- **Do**: Implement the community-join pass, add the community-aggregate dataset to `graph.json`, rewrite the HTML viewer as a two-phase LOD renderer with `forceAtlas2Based` + `hideEdgesOnDrag` [STALE: rewrite is vis-network → sigma.js v3 + WASM-SQLite store, see `design.md`]. Served via `graphos serve`.
- **Check**: Run `graphos <solario-path>` to produce a 78K-node [STALE: 158K-node] `graph.html` [STALE: + `graph.sqlite`], serve it, and verify: (1) initial load < 3s, (2) community overview renders 8K [STALE: 17.6K] dots fluidly, (3) drill-down into a community renders its members in < 500ms, (4) browser memory stays < 1GB [STALE: < 200 MB], (5) `graph.json` `nodes[].community_id` is non-null for community members. Compare against the current freeze-on-load baseline.
- **Act**: If the LOD viewer handles 78K [STALE: 158K] fluidly, standardize the two-phase pattern for future formats (Obsidian, SVG). If it strains at 8K [STALE: 17.6K] community dots, the next PDCA cycle evaluates sigma.js (WebGL) as Fork A from exploration. [STALE: sigma.js is adopted in this cycle per `design.md` Decision 6; the "next cycle" deferral is closed.] If community-join exposes downstream consumers that break on non-null `community_id`, document migration in the node-schema spec.