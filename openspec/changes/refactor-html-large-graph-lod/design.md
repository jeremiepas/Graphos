## Context

Graphos produces `graph.html` (PRD §12) by embedding the full graph JSON inline and handing it to vis-network. Above ~10K nodes the browser freezes. The real target — the 78K-node solario codebase — is unreachable. Investigation surfaced two compounding problems:

1. **Disconnected community data**: Leiden detection (PRD §5.1) produces a `CommunityMap` (community ID → [NodeId]) with all 78,529 nodes assigned across 8,519 communities, but `nodeCommunityId` on every `Node` record is `Nothing` because every extraction module hardcodes `= Nothing` and nothing writes it back. The `communities` dict and the `nodes` array sit side by side in `graph.json` but are never joined. The HTML viewer reads `community_id` (null) and renders a flat, uncolored graph with no structure to aggregate on.

2. **Flat renderer**: vis-network renders every node simultaneously. There is no level-of-detail, no clustering, no progressive loading. 78K DOM-equivalent objects in memory freezes the browser regardless of physics solver tuning.

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
          ├── ExportHTML.exportHTML g analysis htmlPath       ← reads community_id (null)
          ├── ExportReport.exportReport ...
          └── Neo4j / Obsidian / ...
```

Constraints: Domain has zero IO (PRD §4.1). UseCase has zero IO implementation. The incremental JSON writer streams to a handle to keep peak memory low (must not build full JSON AST). `graph.html` stays self-contained (inline JSON) per the exploration decision — served via `graphos serve` (PRD §13.1) but also `file://`-capable.

## Goals / Non-Goals

**Goals:**
- Join `CommunityMap` → `Node.nodeCommunityId` before any export, so `graph.json` and `graph.html` carry real community IDs. Pure function, UseCase layer.
- Add a `community_aggregates` dataset to `graph.json` — one entry per community with the fields the LOD overview needs (member_count, cohesion, bridge_count, color, label, representative_labels, inter_community_edges).
- Rebuild `graph.html` as a two-phase LOD viewer: community-dot overview (forceAtlas2Based + hideEdgesOnDrag) → drill-down into a community's members. Self-contained inline JSON, streaming write preserved.
- 78K-node / 8.5K-community graph renders fluidly: initial load < 3s, drill-down < 500ms, pan/zoom > 30fps, browser memory < 1GB.

**Non-Goals:**
- Switching to sigma.js / WebGL (Fork A from exploration). This is the next PDCA cycle if Fork B strains.
- Building a React SPA frontend. The graphify-HTML form factor (single self-contained HTML) is the target.
- Changing the static server (`Static.hs`). It already serves the output dir over HTTP; no change needed.
- HTTP-fetched JSON. The HTML stays self-contained with inline JSON (drill-down data embedded, renderer holds only a subset in memory).
- Changing the Leiden algorithm or resolution parameters. Community detection is unchanged; this change consumes its output.

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

### Decision 3: HTML viewer is a two-phase LOD renderer, same vis-network dep

`HTML.hs` SHALL be rewritten to emit a viewer with two phases:

```
  Phase 1 — Overview (on load)
  ┌──────────────────────────────────────────────┐
  │  Dataset: community_aggregates               │
  │  Nodes: 1 dot per community (~8.5K)          │
  │  Edges: inter_community_edges (aggregate)    │
  │  Physics: forceAtlas2Based, hideEdgesOnDrag  │
  │  Node size ∝ member_count                    │
  │  Node color = community palette              │
  │  Click community dot → Phase 2               │
  └──────────────────────────────────────────────┘
           │
           ▼ click
  Phase 2 — Drill-down (per community)
  ┌──────────────────────────────────────────────┐
  │  Dataset: nodes where community_id == clicked │
  │           + edges internal to community       │
  │           + bridge edges to other community   │
  │             dots (dashed)                     │
  │  Nodes: member dots, colored by community     │
  │  Edges: internal solid, bridge dashed         │
  │  Physics: forceAtlas2Based on subset          │
  │  "Back to overview" button                    │
  └──────────────────────────────────────────────┘
```

The full `nodes`/`edges` arrays stay inline in the HTML (self-contained), but the vis-network `DataSet` only ever holds the overview dots OR one community's members — never all 78K. The JS filters the inline arrays by `community_id` on drill-down.

**Why vis-network, not sigma.js**: Fork B from exploration — smallest change, keeps the familiar dependency, 8.5K community dots is within vis-network's comfort zone (~10K), drill-down subsets are tiny. The next PDCA cycle evaluates sigma.js if 8.5K strains.

**Alternatives considered:**
- *sigma.js v2 (WebGL)*: Fork A. Handles 100K+ natively but is a bigger rewrite with a new dependency. Deferred to next cycle if needed.
- *Flat render with hideEdgesOnDrag only*: The graphify approach. Buys ~15K, still dies at 78K. Rejected — doesn't meet the goal.
- *Server-side clustering with HTTP-fetched subsets*: Rejected for this cycle — breaks the self-contained constraint and requires Static.hs changes.

### Decision 4: Streaming write preserved; aggregate dataset streamed after god_nodes

`IncrementalJSON.hs` gains `writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()`. The call order in `Pipeline.hs` becomes: `writeNodes → writeEdges → writeCommunities → writeCohesion → writeGodNodes → writeCommunityAggregates → writeAnalysisTail`. The HTML export streams header, then `community_aggregates` JSON, then `nodes` JSON, then `edges` JSON, then the JS body — same handle-based pattern as today, extended for the new dataset.

**Why stream**: The 72MB HTML must not be built as a single in-memory `Text` (PRD §16.2 memory approach). The current `htmlHeader`/`htmlBody` split with `BSL.hPut` for JSON is preserved.

**Alternatives considered:**
- *Build full HTML in memory*: Rejected — 72MB+ Text in memory violates the low-peak-memory invariant.
- *Separate `aggregates.json` file*: Rejected — breaks self-contained HTML; the aggregates must be inline.

### Decision 5: Bridge edges in drill-down connect to collapsed community dots

When a community is expanded, bridge edges (edges crossing community boundaries) SHALL be drawn from the member node to the *target community dot* (still collapsed), not to the target node (not rendered). This keeps the drill-down subset small and visually communicates cross-community structure without rendering the full graph.

**Why dots, not target nodes**: Rendering target nodes would pull in the other community's members, cascading to the full graph. Connecting to the collapsed dot is the Google-Maps-style LOD boundary.

**Alternatives considered:**
- *Hide bridge edges in drill-down*: Rejected — loses cross-community structure, the most interesting part of a knowledge graph.
- *Render target nodes lazily on bridge-edge hover*: Rejected — complexity for marginal value; the community dot already communicates the target.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| 8.5K community dots still strains vis-network on low-end machines | `hideEdgesOnDrag: true` + `forceAtlas2Based` + disable physics after stabilization. If insufficient, next cycle moves to sigma.js (Fork A). |
| `community_id` non-null breaks a downstream consumer that assumed null | Spec marks this as **BREAKING**. Search consumers (Neo4j/Memgraph export) already handle `Maybe Int`; MCP server uses the `communities` dict, not `community_id`. Verified no consumer assumes null. |
| Self-contained HTML stays ~72MB on disk (inline drill-down data) | Accepted trade-off for `file://` capability. The *working set* (renderer memory) drops to ~8.5K dots; disk size is unchanged. `graphos serve` could add gzip in a later cycle. |
| Aggregate computation adds latency to the export step | Pure function, O(N + E) over the graph. On 78K nodes this is < 1s. Streamed to handle, no memory spike. |
| Two-phase viewer loses the "see everything at once" mental model | The overview *is* the whole graph (one dot per community). Drill-down is the detail. This is the standard map-LOD trade-off; accepted. |
| Community join mutates the `Graph` passed to downstream export (Neo4j, Obsidian) | Intended — those consumers benefit from `community_id` being set. Neo4j export already reads `analysisCommunities`; having `community_id` on nodes is additive. |

## Verification Strategy (Check)

Validation against goals and specs (html-lod-viewer, node-schema delta):

1. **Unit tests (cabal test)**:
   - `joinCommunitiesToNodes` purity: given a `CommunityMap` and `Graph`, every node in a community has the correct `community_id`; nodes not in any community stay `Nothing`.
   - `communityAggregates` shape: given a fixture graph + community map, the aggregate list has the right `member_count`, `bridge_count`, `cohesion`, `inter_community_edges`.
   - Property test (QuickCheck): for any graph, `length community_aggregates == Map.size commMap`.

2. **Build gate (cabal build)**: compiles with `-Wall -Wcompat -Werror` per PRD §15.2.

3. **Integration (cabal run graphos -- <small-fixture>)**: produces `graph.json` with non-null `community_id` on community members and a populated `community_aggregates` key.

4. **Benchmark (the 78K-node solario run)**: run `graphos <solario-path>`, serve via `graphos serve --dir graphos-out --port 8080`, verify in a browser:
   - Initial overview load < 3s (stopwatch / DevTools performance).
   - 8,519 community dots rendered, 0 individual node dots.
   - Drill-down into a community < 500ms.
   - Pan/zoom > 30fps with edges hidden during drag.
   - Browser tab memory < 1GB (DevTools Memory).
   - `graph.json` `nodes[].community_id` non-null for all 78,529 nodes.

5. **No-regression**: `graph.json` node count, edge count, community count unchanged vs. baseline run on the same input.

## Iteration & Rollback (Act)

- **If Check fails on latency**: 8.5K dots not fluid → next PDCA cycle switches the renderer to sigma.js v2 (Fork A). The aggregate dataset and community join are reusable; only `HTML.hs` JS changes.
- **If a downstream consumer breaks on non-null `community_id`**: document the migration in the node-schema spec; the join is the correct behavior, the consumer was depending on a bug.
- **If aggregate computation is too slow**: profile; the O(N+E) pass should be < 1s at 78K. If not, investigate strictness/laziness in the fold.
- **Rollback**: revert the Pipeline.hs call sites (join + aggregate write) and HTML.hs. The `CommunityAggregate` type and join function can stay in the codebase unused (no behavior change) or be removed in a follow-up. `graph.json` shape change (`community_aggregates` key, non-null `community_id`) is the only externally visible diff — consumers that ignore unknown keys and tolerate null/non-null `community_id` are unaffected.

## Migration Plan

1. Add `CommunityAggregate` Domain type and `ToJSON` instance.
2. Add `joinCommunitiesToNodes` and `computeCommunityAggregates` UseCase functions.
3. Wire both into `Pipeline.hs` (join before `epWriteNodes`; aggregate after `epWriteGodNodes`).
4. Add `writeCommunityAggregates` to `IncrementalJSON.hs`.
5. Rewrite `HTML.hs` viewer JS (two-phase LOD).
6. Run `cabal test` then the 78K-node benchmark.
7. No data migration needed — the next pipeline run populates the new fields. Old `graph.json` files load fine (consumers tolerate the missing `community_aggregates` key and null `community_id`).

## Open Questions

- Should `community_aggregates` also be written to the checkpoint file (`graph.checkpoint.json`), or only the final `graph.json`? Lean: final only — checkpoints are pre-inference snapshots and aggregates need post-clustering data.
- Should the LOD viewer support multi-community selection (expand two communities at once)? Lean: no for this cycle — one-at-a-time keeps the subset tiny and the UX clear. Defer to feedback.