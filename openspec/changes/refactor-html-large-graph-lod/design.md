## Context

Graphos produces `graph.html` (PRD §12) by embedding the full graph JSON inline and handing it to vis-network. Above ~10K nodes the browser freezes. The real target — the solario codebase — is unreachable. **Scale reality (measured 2026-08-10 on `../../solario/test/graph.html`): 158,166 nodes, 184,616 edges, 17,651 communities, 157 MB HTML on disk. This is 2× the scale the original design assumed (78K / 8.5K / 72 MB).** Three compounding problems surfaced during investigation:

1. **Disconnected community data**: Leiden detection (PRD §5.1) produces a `CommunityMap` (community ID → [NodeId]) with all nodes assigned, but `nodeCommunityId` on every `Node` record is `Nothing` because every extraction module hardcodes `= Nothing` and nothing writes it back (fixed by `optimise-community-detection-large-graph` unstubbing Step 5). The `communities` dict and the `nodes` array sit side by side in `graph.json` but are never joined.

2. **Flat renderer + inline-everything**: vis-network renders every node simultaneously, and the HTML inlines all 158K nodes + 184K edges as JSON. The browser parses 157 MB and holds ~1–1.5 GB heap for the whole session so drill-down can filter client-side. At 158K this freezes the page on load (parse-bound), before a single dot is visible.

3. **Spec-vs-code drift (four bugs, see Decision 9)**: the shipped `HTML.hs` and `Cluster.hs` diverge from the `html-lod-viewer` spec in load-bearing ways — the `inter_community_edges` field is a scalar count instead of a pair list, the HTML path uses a stubbed duplicate type, overview edges are recomputed from 184K edges on load, and drill-down silently drops bridge edges.

Current architecture (relevant slice):

```
  Pipeline.hs (UseCase)
    │
    ├── Step 4: clusterGraphWithResolution graph res
    │     → (commMap, cohesion) :: (CommunityMap, CohesionMap)
    │
    ├── Step 5: re-cluster → (finalComm, finalCohes), analyzeGraph → anal
    │
    ├── epWriteNodes iw (Map.elems (gNodes enrichedGraph'))   ← nodes have community_id = Nothing
    ├── epWriteCommunities iw finalComm                       ← communities written separately
    ├── epWriteCohesion iw finalCohes
    ├── epWriteGodNodes iw (analysisGodNodes anal)
    │
    └── exportAll enrichedGraph' anal config detection mLabels
          ├── ExportHTML.exportHTML g analysis htmlPath       ← inlines 157 MB JSON
          ├── ExportReport.exportReport ...
          └── Neo4j / Obsidian / ...
```

Constraints: Domain has zero IO (PRD §4.1). UseCase has zero IO implementation. The incremental JSON writer streams to a handle to keep peak memory low (must not build full JSON AST). **Revised (2026-08-10): `graph.html` no longer stays self-contained with inline JSON — the 157 MB inline payload is the failure mode. The HTML carries only the aggregate dataset; member data lives in a `graph.sqlite` file served via `graphos serve` (PRD §13.1) and queried by `@sqlite.org/sqlite-wasm` with OPFS. `file://` capability is dropped.**

## Goals / Non-Goals

**Goals:**
- Join `CommunityMap` → `Node.nodeCommunityId` before any export, so `graph.json` and `graph.html` carry real community IDs. Pure function, UseCase layer.
- Add a `community_aggregates` dataset to `graph.json` — one entry per community with the fields the LOD overview needs (member_count, cohesion, bridge_count, color, label, representative_labels, inter_community_edges as a **list of `(target, count)` pairs** per Decision 8).
- Rebuild `graph.html` as a small (~2 MB) two-phase LOD viewer: community-dot overview → drill-down into a community's members, with member data fetched on click from a `graph.sqlite` store via `@sqlite.org/sqlite-wasm` + OPFS. Renderer is sigma.js v3 (WebGL).
- Emit `graph.sqlite` alongside `graph.json` — nodes, edges, and `community_edge_pairs` tables, streamed via `direct-sqlite` in batched transactions.
- `graphos serve` emits COOP/COEP cross-origin isolation headers so OPFS is available.
- 158K-node / 17.6K-community graph renders fluidly: initial load < 3s, drill-down < 500ms, pan/zoom > 30fps, browser memory < 200 MB (down from >1 GB).

**Non-Goals:**
- Keeping `file://` capability. The inline-JSON `file://` form is what broke at 158K; `graphos serve` is the primary path.
- Keeping vis-network. Replaced by sigma.js v3 (Decision 6).
- Keeping the self-contained inline-JSON HTML. The full graph moves to `graph.sqlite` (Decisions 3, 4).
- Building a React SPA frontend. The form factor is still a single HTML file + a db file + a static server; no build step, no framework.
- Changing the Leiden algorithm or resolution parameters. Community detection is unchanged; this change consumes its output.
- Replacing `graph.json` for non-HTML consumers (MCP server, Neo4j export). `graph.json` stays the interchange format; `graph.sqlite` is the HTML viewer's store.

## Decisions

### Decision 1: Community join is a pure UseCase function operating on `Graph`

The join `CommunityMap → Graph` (setting `nodeCommunityId` on each `Node`) SHALL be a pure function in the UseCase layer, invoked in `Pipeline.hs` between re-clustering (line 263) and `epWriteNodes` (line 269). It produces a new `Graph` with updated `Node` records. Domain stays pure (no new Domain code beyond what exists); UseCase orchestrates the join using the existing `CommunityMap` and `Graph` types.

**Why UseCase, not Domain**: The join consumes a `CommunityMap` (Domain type) and a `Graph` (Domain type) and produces a `Graph`. It is pure orchestration of Domain types — exactly the UseCase layer's role (PRD §4.1). Putting it in Domain would couple Domain to the "post-clustering join" workflow concept.

**Alternatives considered:**
- *Join in Infrastructure (Export layer)*: Rejected — would require re-reading `communities` from the written JSON to fix `nodes`, doubling the export pass and breaking the single-stream incremental writer.
- *Join in Domain (Analysis.hs)*: Rejected — `Analysis` is a read-only summary type; mutating `Graph` belongs to the pipeline orchestration.
- *Join at HTML export only*: Rejected — `graph.json` would still ship `community_id: null`, breaking the spec requirement and any JSON consumer.

### Decision 2: `community_aggregates` computed in UseCase, serialized by IncrementalJSON

A new pure UseCase function SHALL compute the aggregate dataset from `(Graph, CommunityMap, CohesionMap, [NodeId] articulationPoints, Maybe communityLabels)`. It returns a `[CommunityAggregate]` (new Domain type in `Domain.Types.Analysis` or a new `Domain.Types.CommunityAggregate`). The IncrementalJSON writer gains a `writeCommunityAggregates` step, called after `writeGodNodes` in `Pipeline.hs`.

**Why a new type, not reuse `CommunityGraph`**: `CommunityGraph.hs` already produces a community-level graph for `community_graph.json` (a separate optional export). Its shape differs (nodes/edges graph structure vs. the flat aggregate the HTML overview needs). Reusing it would force the HTML viewer to do a join it shouldn't care about. A dedicated `CommunityAggregate` type keeps the HTML overview self-contained.

**Alternatives considered:**
- *Reuse `community_graph.json`*: Rejected — different shape, optional flag-gated output, and would couple the HTML viewer to a separate file.
- *Compute aggregates in JS (browser-side)*: Rejected — 78K nodes in browser memory to compute 8.5K aggregates defeats the LOD goal.
- *Compute in Infrastructure (HTML.hs)*: Rejected — HTML export shouldn't compute analysis; it should consume it.

### Decision 3 (REVISED 2026-08-10): Two-phase LOD viewer backed by a WASM-SQLite store, rendered with sigma.js v3

**What changed.** The original Decision 3 kept `graph.html` self-contained with the full graph inlined as JSON and filtered client-side. The benchmark input turned out to be **2× the assumed scale**: 158,166 nodes / 184,616 edges / 17,651 communities (vs. the 78K / 8.5K the decision was written against), producing a **157 MB** HTML file. Profiling the current `../../solario/test/graph.html` in a browser confirmed the failure mode the original decision deferred:

```
  Browser opens 157 MB HTML
        │
        ▼
  ┌─────────────────────────────────────────────────┐
  │ JSON.parse 158K-node + 184K-edge inline arrays  │  ~5–15 s, ~1–1.5 GB heap
  │ Build allNodes[] / allEdges[] / nodeCommMap{}    │  held for the whole session
  │   / commToNodes{} / commToEdges{}                │  so drill-down can filter
  │ buildOverviewEdges(): scan all 184K edges        │  O(E) before the first dot
  ├─────────────────────────────────────────────────┤
  │ vis-network renders 17.6K community dots        │  the only visible part
  └─────────────────────────────────────────────────┘
```

The renderer working set is small (~17.6K dots) but the browser heap holds the entire graph for the whole session so drill-down can filter it. At 158K this is the wall — the page freezes on load (parse-bound), and tab memory crosses 1 GB before the user sees anything. The self-contained constraint was the only reason to inline everything; at 2× the assumed scale that tradeoff flips.

**New architecture.** The graph data leaves the HTML. The HTML carries only the aggregate dataset; member nodes and edges live in a SQLite database file served via `graphos serve` and queried on demand by `@sqlite.org/sqlite-wasm` with OPFS-backed paging. The renderer is sigma.js v3 (WebGL) instead of vis-network.

```
  Export time:
    graph.html              ← ~2 MB: viewer JS + community_aggregates inline
    graph.sqlite            ← ~66 MB: nodes / edges / community_edge_pairs tables
    graph.json              ← unchanged, full export for non-HTML consumers

  Load (via graphos serve):
    GET /graph.html
        parse 17.6K aggregates → render dots immediately (<1 s, ~50 MB heap)
    user clicks community 4
        SELECT * FROM nodes WHERE community_id = 4        → ~17 rows
        SELECT * FROM edges WHERE src_community = 4
               OR tgt_community = 4                       → internal + bridge
        sigma graph drops community 4 dot, adds member nodes
        + dashed bridge edges to collapsed community dots
    "Back to overview" reverses the drop
```

**Why sigma.js v3 over vis-network.** vis-network tops out around ~10K nodes in a single `DataSet`; the original decision only held at 8.5K community dots. At 17.6K it is past comfort even for the overview, and any future "expand two communities at once" exploration pushes it further. sigma.js v3 (3.0.3) + graphology (0.26.0) render with WebGL, are designed for 100K+ graphs, and support partial refresh on graph mutations (`addNode`/`dropNode` events trigger incremental re-render without a full rebuild) — exactly the drop-in/drop-out pattern drill-down needs. `clickNode` + `camera.animate({x,y},{duration:500})` give the click-to-zoom behaviour the spec requires.

**Why `@sqlite.org/sqlite-wasm` with OPFS, not `sql.js` or sharded JSON.** Three options were compared for the client-side store:

| | `sql.js` (pure WASM) | `@sqlite.org/sqlite-wasm` + OPFS | Sharded JSON (`community/<cid>.json`) |
|---|---|---|---|
| Load cost | parse whole db into `Uint8Array`, ~2.5× db size peak (~166 MB) | open handle, page in on query, only touched pages in memory | fetch one small file per drill-down |
| Steady heap | ~66 MB (full db resident) | a few MB page cache | tiny + grows per drill-down |
| Query power | full SQL (WHERE community_id, JOIN edges) | full SQL | none — pre-split at export |
| `file://` works | no (needs fetch for the db) | no (needs OPFS + COOP/COEP) | no (needs fetch) |
| Schema flexibility | high (can re-query for any future view) | high | low (export must pre-compute every cut) |
| Export cost | one .sqlite file | one .sqlite file | 17,651 tiny files |

SQLite-with-OPFS wins on three counts: (1) it scales to any future query without re-exporting — search, multi-community expansion, neighbour-of-neighbour exploration all become SQL; (2) the WASM SQLite page cache keeps the steady heap at a few MB instead of ~66 MB resident; (3) one artifact instead of 17K shards. The cost is that `graphos serve` must serve with COOP/COEP cross-origin isolation headers (a small additive change to `Static.hs`). `file://` capability is dropped — `graphos serve` is already the spec's primary delivery path (`html-lod-viewer/spec.md:89`), and the inline-JSON `file://` form is the thing that broke at 158K.

**The two phases are unchanged in shape.** Overview = community dots positioned by inter-community edges, sized by `member_count`, colored by palette. Drill-down = member nodes + internal edges + dashed bridge edges to collapsed community dots. The only difference is *where the data comes from on drill-down* (SQL query vs. inline-array filter) and *which renderer draws it* (sigma vs. vis-network).

**Alternatives considered:**
- *Keep inline JSON, switch to sigma.js only*: renderer-side fix; the 157 MB parse + 1+ GB heap on load remains. Rejected — the wall is parse + heap, not render.
- *Inline JSON with lazy chunked parse* (e.g. `JSON.parse` on byte slices): keeps `file://`, but the 157 MB string still ships in the HTML and JS has no native streaming JSON parser. Rejected — complexity for marginal gain, still holds the full string.
- *sql.js (pure WASM, no OPFS)*: simpler (no COOP/COEP) but loads the whole db into memory, defeating the point. Rejected — peak ~166 MB.
- *Sharded JSON via `graphos serve`*: simplest server, no WASM, but pre-computes every cut at export and explodes to 17K files. Rejected — inflexible, and the file count is its own scaling problem.
- *Flat render with `hideEdgesOnDrag` only*: the graphify approach. Buys ~15K, dies at 158K. Rejected — doesn't meet the goal.
- *Keep self-contained HTML, accept the freeze*: the original Decision 3. Rejected — the benchmark (Task 7) cannot pass at 158K.

### Decision 4 (REVISED 2026-08-10): Streaming write preserved; HTML is small, SQLite db is the new streaming target

The HTML export no longer inlines the full graph, so it shrinks from ~72–157 MB to ~2 MB (viewer JS + the aggregate dataset). The streaming-to-handle constraint still applies, but the heavy streaming target moves to the new SQLite export.

`IncrementalJSON.hs` gains `writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()` as before (called after `writeGodNodes`). The HTML export streams header, then `community_aggregates` JSON (small), then the JS body — same handle-based pattern, no more inline `nodes`/`edges` arrays.

A new `Infrastructure/Export/SQLite.hs` module SHALL stream nodes, edges, and community-edge-pairs into a `graph.sqlite` file via the Haskell `direct-sqlite` package (already in the dependency tree — verify). Rows are inserted in batches inside a transaction to keep peak memory flat:

```
  tables: nodes(id TEXT PRIMARY KEY, label TEXT, source_file TEXT,
               file_type TEXT, kind TEXT, community_id INT,
               is_bridge INT, degree INT)
         edges(id TEXT, src TEXT, tgt TEXT, relation TEXT,
               confidence REAL, src_community INT, tgt_community INT)
         community_edge_pairs(src_cid INT, tgt_cid INT, count INT,
                              PRIMARY KEY(src_cid, tgt_cid))
  indexes: nodes(community_id), edges(src_community), edges(tgt_community)
```

`src_community` / `tgt_community` are denormalized onto `edges` at export time so drill-down queries (`WHERE src_community = ? OR tgt_community = ?`) are single-table reads with an index — no join needed per click.

**Why a real db file, not in-memory construction**: the 66 MB db must not be built as a single in-memory structure (PRD §16.2). `direct-sqlite` streams rows to the file handle via SQLite's own paging — peak Haskell heap stays at the batch size (~1K rows), not the db size.

**Alternatives considered:**
- *Build the SQLite db in JS in the browser* (ship CSV, import client-side): rejected — re-parses the full dataset on every load, the exact cost we're removing.
- *Keep `graph.json` and have the browser fetch + filter it*: rejected — fetch transfers 190 MB and `JSON.parse` of 190 MB is the wall we're escaping.
- *Skip SQLite, ship pre-split per-community JSON shards*: rejected for the inflexibility reasons in Decision 3 (every new query cut needs a re-export).

### Decision 5 (REVISED 2026-08-10): Bridge edges in drill-down are a first-class SQL query, fixing the dropped-bridge drift bug

When a community is expanded, bridge edges (edges crossing community boundaries) SHALL be drawn from the member node to the *target community dot* (still collapsed), not to the target node. The drill-down JS issues one SQL query returning both internal and bridge edges for the community:

```sql
  SELECT * FROM edges
   WHERE src_community = ? OR tgt_community = ?
```

For each returned edge: if both endpoints are in the expanded community → solid internal edge; if one endpoint is outside → dashed bridge edge to the collapsed target community dot. The query is index-backed (`edges(src_community)`, `edges(tgt_community)`) and returns a small set (community mean = 9 members, so typically <50 edges).

**Drift bug captured (see Decision 9).** The shipped `buildDrilldownData` (`HTML.hs:310-316`) filters `e => memberIds.has(e.from) && memberIds.has(e.to)` — **internal edges only**, silently dropping every bridge edge. This is a spec violation baked into the current code. The SQL-backed drill-down fixes it structurally: the query returns both classes and the JS distinguishes them, instead of the filter discarding one class.

**Why dots, not target nodes**: rendering target nodes would pull in the other community's members, cascading to the full graph. Connecting to the collapsed dot is the Google-Maps-style LOD boundary — unchanged from the original Decision 5.

**Alternatives considered:**
- *Hide bridge edges in drill-down*: rejected — loses cross-community structure, the most interesting part of a knowledge graph.
- *Render target nodes lazily on bridge-edge hover*: rejected — complexity for marginal value; the community dot already communicates the target.

### Decision 6: Renderer is sigma.js v3 + graphology, replacing vis-network

The HTML viewer SHALL depend on `sigma` (3.0.3) + `graphology` (0.26.0) instead of `vis-network`. Both ship as ES modules and load from a CDN (or are vendored under `graphos serve` for offline use).

**Behaviour mapping (vis-network → sigma.js):**

| vis-network (current) | sigma.js v3 (new) |
|---|---|
| `new vis.Network(container, {nodes, edges}, options)` | `new Sigma(graphologyGraph, container, settings)` |
| `vis.DataSet` add/remove for drill-down | `graph.addNode` / `graph.dropNode` — sigma partial-refreshes |
| `physics: forceAtlas2Based` | `forceAtlas2` from `graphology-layout-forceatlas2` (run N iterations to lay out, then stop) |
| `click` → `params.nodes[0]` | `clickNode` event → `event.data.node` |
| `network.focus(id, {scale, animation})` | `renderer.getCamera().animate({x,y}, {duration:500})` using `renderer.getNodeDisplayData(id)` |
| `hideEdgesOnDrag` | `settings.hideEdgesOnMove` (sigma hides edges on any move, including zoom) |

**Why now, not deferred.** The original Decision 3 deferred sigma.js to "the next PDCA cycle if 8.5K strains." 17.6K community dots is past vis-network's comfort zone regardless of the storage change; the renderer swap and the storage swap are independent and both required. Deferring sigma again would mean revisiting the renderer in the very next cycle.

**Alternatives considered:**
- *vis-network at 17.6K with aggressive opts*: marginal — 17.6K is already past the ~10K comfort band; pan/zoom fps would not meet the >30fps target.
- *cytoscape.js*: canvas-based, comparable scale limits to vis-network; no benefit over sigma for this scale.
- *d3-force + custom WebGL*: maximum control, maximum cost; rejected for this cycle.

### Decision 7: `graphos serve` emits COOP/COEP cross-origin isolation headers

`@sqlite.org/sqlite-wasm` OPFS requires `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp` on every response, otherwise `navigator.storage.getDirectory()` (OPFS entry point) is unavailable and the SQLite module falls back to in-memory mode (which re-introduces the full-db-in-memory cost).

`Static.hs` SHALL add these two headers to every response (both 200 and 404), alongside the existing `Access-Control-Allow-Origin: *`. This is a ~4-line change to `staticApp`.

**Why `*` CORS with `same-origin` COOP**: the two are compatible — COOP/COEP isolate the browsing context, `Access-Control-Allow-Origin: *` permits cross-origin reads of the static files. The combination is the documented pattern for OPFS-enabled static sites.

**Drop `file://` capability.** Opening `graph.html` directly from the filesystem no longer works — the SQLite module cannot load, and even if it could, OPFS is not available under `file://`. This is the accepted tradeoff from Decision 3. `graphos serve` is the primary path; the spec already requires it (`html-lod-viewer/spec.md:89-96`).

**Alternatives considered:**
- *Ship a `graph.html` that works under `file://` by falling back to inline JSON if OPFS is unavailable*: rejected — would require shipping the full inline JSON anyway (the 157 MB we're escaping), and the fallback path is the broken path.
- *Use `sql.js` (no OPFS, no COOP/COEP) and accept ~66 MB resident*: rejected — Decision 3 comparison showed the steady-heap cost defeats the purpose.

### Decision 8: `community_aggregates.inter_community_edges` is a list of pairs, not a scalar count — spec honored, drift fixed

The `html-lod-viewer` spec (`spec.md:45`) requires `inter_community_edges` to be a list of `(target community id, edge count)` pairs. The shipped Domain type `CommunityAggregate.caInterCommunityEdges :: Int` (`Analysis.hs:93`) holds only a scalar count — the per-target breakdown is computed in `Cluster.hs:108-119` (`interEdgeCounts :: Map CommunityId (Map CommunityId Int)`) and then **discarded** at line 142 (`Map.size ...` keeps only the count of distinct targets, not the pairs).

**Fix.** Change the type:

```haskell
  -- Domain.Types.Analysis
  , caInterCommunityEdges :: ![(CommunityId, Int)]   -- was: !Int
```

`Cluster.hs:142` returns `Map.toList (Map.findWithDefault Map.empty cid interEdgeCounts)` instead of `Map.size ...`. `ToJSON`/`FromJSON` serialize as `[{"target": <cid>, "count": <n>}, ...]` (snake_case: `inter_community_edges`). The HTML overview reads this list directly to draw inter-community edges — eliminating the client-side 184K-edge scan (`buildOverviewEdges` in `HTML.hs:285-307` is deleted).

A new `community_edge_pairs` table in `graph.sqlite` (Decision 4) also holds `(src_cid, tgt_cid, count)` rows so the JS could re-derive overview edges from the db if needed, but the inline `community_aggregates` list is the primary source (smaller initial load).

**Alternatives considered:**
- *Keep scalar `Int`, compute pairs client-side from `graph.sqlite`*: rejected — adds a query at load time before the overview can render; the inline list is small (8.5K × avg-2-targets ≈ 17K pairs) and belongs in the aggregate.

### Decision 9: Four spec-vs-code drift bugs are explicit work items

Reading the shipped `HTML.hs` and `Cluster.hs` against the spec surfaced four places where the code diverges from the spec in load-bearing ways. Each is fixed by the revised architecture above; they are enumerated here so Task 6 (HTML rewrite) and the new SQLite-export task carry explicit sub-checks.

1. **`inter_community_edges` shape** — scalar `Int` instead of `[(targetCid, count)]`. Fixed by Decision 8. Spec: `html-lod-viewer/spec.md:45`.

2. **HTML path uses a stubbed duplicate type.** `HTML.hs` defines its own `VisCommunityAggregate` (`HTML.hs:757-778`) with `vcaInterCommunityEdges = 0` hardcoded (`HTML.hs:845`), divergent from the real `Domain.CommunityAggregate`. The HTML's `_communityAggregatesData` is therefore wrong for every community. Fixed by deleting `VisCommunityAggregate` and emitting the real `CommunityAggregate` JSON (the HTML no longer needs a viewer-specific type — it consumes the canonical Domain type).

3. **Overview edges recomputed client-side from 184K edges.** `buildOverviewEdges` (`HTML.hs:285-307`) scans `allEdges` on load to find cross-community pairs — an O(E) scan before the first dot renders. The aggregate dataset was supposed to eliminate this. Fixed by Decision 8 (the inline `inter_community_edges` list drives overview edges directly) plus Decision 3 (no more inline `allEdges` at all).

4. **Drill-down drops bridge edges.** `buildDrilldownData` (`HTML.hs:310-316`) filters `memberIds.has(e.from) && memberIds.has(e.to)` — internal edges only. Spec requires dashed bridge edges to collapsed community dots. Fixed by Decision 5 (the SQL query returns both classes).

These are captured as explicit check criteria in the revised tasks (Task 6 check; new SQLite-export task check).

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| 17.6K community dots strains any renderer on low-end machines | sigma.js v3 WebGL + `hideEdgesOnMove`; disable forceAtlas2 after stabilization. If insufficient, next cycle evaluates LOD-on-the-GPU (instanced nodes) or coarser clustering (fewer, bigger communities). |
| OPFS unsupported (old browser / non-secure context) | Detect at load; show a "OPFS required, use a modern browser via `graphos serve`" message. Do not fall back to inline JSON (that path is the broken path). |
| `graphos serve` COOP/COEP headers break a consumer embedding the HTML in an iframe | Acceptable — `graph.html` is a standalone viewer, not an embeddable widget. If embedding becomes a requirement, a later cycle serves a non-isolated variant without OPFS. |
| `community_id` non-null breaks a downstream consumer that assumed null | Spec marks this as **BREAKING**. Search consumers (Neo4j/Memgraph export) already handle `Maybe Int`; MCP server uses the `communities` dict, not `community_id`. Verified no consumer assumes null. |
| `graph.sqlite` is a new export artifact; consumers expecting only `graph.json` are surprised | Add to docs; `graph.json` is unchanged in shape (still has full nodes/edges/communities). `graph.sqlite` is additive for the HTML viewer. |
| `direct-sqlite` not in the dependency tree | Verify at Task start; if absent, add via cabal. `direct-sqlite` is the standard binding; low risk. |
| Aggregate computation adds latency to the export step | Pure function, O(N + E). On 158K nodes < 2 s. Streamed to handle, no memory spike. |
| SQLite export adds latency (158K inserts) | Batched inserts in a transaction: ~1–2 s for 158K rows on SSD. Measured at Check. |
| Two-phase viewer loses the "see everything at once" mental model | The overview *is* the whole graph (one dot per community). Drill-down is the detail. Standard map-LOD trade-off; accepted. |
| Community join mutates the `Graph` passed to downstream export (Neo4j, Obsidian) | Intended — those consumers benefit from `community_id` being set. Neo4j export already reads `analysisCommunities`; having `community_id` on nodes is additive. |
| `file://` capability is dropped | Accepted. The inline-JSON `file://` form is what broke at 158K. `graphos serve` is the spec's primary path. Document in the README. |

## Verification Strategy (Check)

Validation against goals and specs (html-lod-viewer, node-schema delta):

1. **Unit tests (cabal test)**:
   - `joinCommunitiesToNodes` purity: given a `CommunityMap` and `Graph`, every node in a community has the correct `community_id`; nodes not in any community stay `Nothing`.
   - `communityAggregates` shape: given a fixture graph + community map, the aggregate list has the right `member_count`, `bridge_count`, `cohesion`, and `inter_community_edges` as a list of `(target, count)` pairs (Decision 8 fix).
   - Property test (QuickCheck): for any graph, `length community_aggregates == Map.size commMap`.
   - SQLite export round-trip: write a fixture graph to a temp `graph.sqlite`, reopen with `direct-sqlite`, assert row counts and one sample row per table.

2. **Build gate (cabal build)**: compiles with `-Wall -Wcompat -Werror` per PRD §15.2.

3. **Integration (cabal run graphos -- <small-fixture>)**: produces `graph.json` (unchanged shape, non-null `community_id`), `graph.sqlite` (new, queryable), and `graph.html` (small, ~2 MB, references the db).

4. **Drift-bug checks (Decision 9)**:
   - `graph.json` `community_aggregates[0].inter_community_edges` is a list of `{"target":..,"count":..}` objects, not a scalar. (bug 1)
   - `HTML.hs` no longer defines `VisCommunityAggregate`; the HTML consumes the canonical `CommunityAggregate` JSON. (bug 2)
   - The generated `graph.html` contains no `buildOverviewEdges` function (overview edges come from the inline aggregate list). (bug 3)
   - Drill-down into a community with known bridge edges shows dashed edges to collapsed community dots. (bug 4)

5. **Benchmark (the 158K-node solario run)**: run `graphos <solario-path>`, serve via `graphos serve --dir graphos-out --port 8080`, verify in a browser:
   - Initial overview load < 3 s (DevTools performance).
   - 17,651 community dots rendered, 0 individual node dots.
   - Drill-down into a community < 500 ms (SQL query + sigma partial refresh).
   - Pan/zoom > 30 fps with `hideEdgesOnMove` enabled.
   - Browser tab memory < 200 MB (DevTools Memory) — down from >1 GB.
   - `graph.json` `nodes[].community_id` non-null for all 158,166 nodes.
   - `graph.sqlite` present, ~66 MB, queryable.

6. **COOP/COEP check**: `curl -I http://localhost:8080/graph.html` shows `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp`.

7. **No-regression**: `graph.json` node count, edge count, community count unchanged vs. baseline run on the same input.

## Iteration & Rollback (Act)

- **If Check fails on latency at 17.6K dots**: sigma.js v3 should handle this natively; if not, the next PDCA cycle evaluates WebGL instanced rendering or coarser community resolution (fewer, bigger communities via Leiden `resolution` parameter).
- **If OPFS is unavailable on the target browser**: document the browser support matrix; do not add the inline-JSON fallback (that path is the broken path). If a non-OPFS path is truly required, the next cycle evaluates `sql.js` with the ~66 MB resident cost.
- **If `direct-sqlite` is not available or the SQLite export is too slow**: evaluate `persistent-sqlite` or a raw CSV+`sql.js`-import-in-worker path. The SQLite schema is simple; the binding choice is reversible.
- **If a downstream consumer breaks on non-null `community_id`**: document the migration in the node-schema spec; the join is the correct behavior, the consumer was depending on a bug.
- **Rollback**: revert the Pipeline.hs call sites (join + aggregate write + SQLite export), `HTML.hs`, `Static.hs` headers, and the new `SQLite.hs`. `graph.json` shape change (`community_aggregates` key with list-valued `inter_community_edges`, non-null `community_id`) is the only externally visible diff to existing consumers. `graph.sqlite` is additive — removing it breaks only the HTML viewer.

## Migration Plan

1. (Done in tasks 1–5) Add `CommunityAggregate` Domain type, `joinCommunitiesToNodes`, `computeCommunityAggregates`; wire into `Pipeline.hs`; add `writeCommunityAggregates` to `IncrementalJSON.hs`.
2. **Fix the `inter_community_edges` type** (Decision 8): change `caInterCommunityEdges :: Int` to `![(CommunityId, Int)]`, update `Cluster.hs` to keep the per-target map, update `ToJSON`/`FromJSON`.
3. **Add `Infrastructure/Export/SQLite.hs`** (Decision 4): stream nodes, edges, `community_edge_pairs` to `graph.sqlite` in batched transactions. Add `direct-sqlite` to `.cabal` if absent.
4. **Wire SQLite export into `Pipeline.hs`** after the JSON export, same `Graph` input.
5. **Update `Static.hs`** (Decision 7): add COOP/COEP headers to every response.
6. **Rewrite `HTML.hs`** (Decisions 3, 6, 9): delete `VisCommunityAggregate` and `buildOverviewEdges`; emit a small HTML with inline `community_aggregates`, sigma.js + graphology + `@sqlite.org/sqlite-wasm` from CDN; JS opens OPFS db, renders overview from aggregates, queries the db on drill-down.
7. Run `cabal test` then the 158K-node benchmark (Task 7).
8. No data migration — the next pipeline run produces the new artifacts. Old `graph.json` files load fine (consumers tolerate the missing/list-valued `inter_community_edges` and null/non-null `community_id`). Old `graph.html` files still open (they're self-contained, just frozen at 158K).

## Open Questions

- Should `community_aggregates` also be written to the checkpoint file (`graph.checkpoint.json`), or only the final `graph.json`? Lean: final only — checkpoints are pre-inference snapshots and aggregates need post-clustering data.
- Should the LOD viewer support multi-community selection (expand two communities at once)? With SQL-backed drill-down this is cheap (UNION two queries) — re-evaluate after the single-community path is validated.
- Vendor sigma.js / sqlite-wasm under `graphos serve` (offline) or always CDN? Lean: vendor — removes a runtime network dependency, aligns with the "self-contained output dir" principle even though `file://` is gone.
- Should `graph.sqlite` replace `graph.json` for non-HTML consumers (MCP server, Neo4j export)? Lean: no for this cycle — `graph.json` is the documented interchange format; `graph.sqlite` is the HTML viewer's store. Re-evaluate if a consumer wants SQL access.