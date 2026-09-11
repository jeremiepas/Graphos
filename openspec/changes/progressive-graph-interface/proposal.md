# progressive-graph-interface

## Why

The viewer stops being usable long before graphs stop being useful. `graph.html` embeds the
entire payload inline (required today by `html-lod-viewer`'s self-contained rule): the current
reference corpus already produces a 91 MB document, and real targets reach **4.4 GB** of
`graph.json` — no browser will parse that, let alone keep it on the vis-network canvas. The
interface also lags Obsidian's graph view, which users name as the reference experience, in two
places that matter daily:

1. **No query-defined groups.** Obsidian lets you type a query, give it a color, and instantly
   see that cohort in the graph. Graphos has a scored query engine (`/api/query`) and facets,
   but no way to say "color everything matching `auth token` orange, tests grey" and keep those
   cohorts live while exploring.
2. **Frozen rendering parameters.** Physics, node scaling, label density and LOD budgets are
   compile-time constants in the viewer assets. Tuning the feel for a given graph — the thing
   Obsidian's sliders make trivial — requires editing source and rebuilding.

The pieces to fix this already exist: `community_aggregates` gives a bounded overview,
`GraphIndex` gives O(k) lookup and precomputed adjacency server-side, and `graphos serve`
already shares one in-memory graph across `/api/*`. What is missing is a slice-serving API, a
viewer that fetches instead of embeds, and the two Obsidian-grade interaction surfaces on top.

## What Changes

- `graphos serve` SHALL expose a **slice API**: bounded endpoints for overview aggregates,
  paged community subgraphs, and node neighborhoods — every response hard-capped and carrying
  totals, so the client always knows what it has not yet loaded. No endpoint ever returns the
  whole graph.
- The viewer SHALL gain a **remote mode**: start from aggregates only, fetch slices on
  drill-down and expansion, evict least-recently-viewed slices under a client memory budget.
  Embedded mode remains for small graphs and `file://`; the HTML export chooses by payload
  threshold. **BREAKING (spec-level)**: `html-lod-viewer`'s unconditional "self-contained"
  requirement becomes conditional on graph size.
- **Query groups**, Obsidian-style: named query + color, evaluated server-side over the *full*
  graph via the existing query engine, first-match-wins precedence, live counts, isolate/hide
  toggles, persisted per graph and exportable.
- A **live tuning panel**: physics (gravity, link distance, repulsion), visuals (node scale by
  degree, label threshold, edge opacity), and budgets (slice page size, LOD cap) — applied
  without reload or re-mount, persisted, one-click reset to defaults.

## Capabilities

### New Capabilities

- `graph-slice-api`: HTTP endpoints on `graphos serve` returning bounded, cursor-paged slices
  (aggregates / community / neighborhood) of the shared in-memory graph, with totals and
  explicit truncation flags.
- `progressive-graph-viewer`: the fetch-on-demand viewer mode — aggregates-first load, slice
  fetch on navigation, client-side memory budget with LRU eviction and visible load state.
- `query-groups`: named query+color groups over the full graph, Obsidian-semantics precedence,
  counts, isolate/hide, per-graph persistence and JSON import/export.
- `live-render-tuning`: the live parameter panel — physics, visual and budget parameters
  adjustable at runtime with immediate effect, persisted, resettable.

### Modified Capabilities

- `html-lod-viewer`: the self-contained rule becomes size-conditional — embed the payload below
  a threshold, emit a remote-mode document (assets inline, data fetched) above it. Overview
  data source (`community_aggregates`) and LOD phase semantics unchanged.
- `query-http-port`: the server additionally exposes the slice and group-evaluation endpoints,
  sharing the existing static + `/api/*` routing, CORS and 405 behavior.

## Impact

- **Code**: `Graphos.Infrastructure.Server.QueryAPI`/`Static` (slice + group routes over the
  existing `SharedLoadRef`/`GraphIndex`), `Graphos.Infrastructure.Export.HTML` (mode selection,
  no-embed emission), `assets/viewer/viewer.js` + `viewer.css` (remote data layer, groups UI,
  tuning panel), `Graphos.UseCase.Query` (group evaluation reusing scoring).
- **APIs**: new `/api/*` read-only endpoints; `/api/query` family unchanged.
- **Dependencies**: none new — Warp, Aeson, vis-network and `GraphIndex` suffice.
- **Out of scope** (follow-up change if needed): on-disk/streaming server storage. The server
  keeps its in-memory model (`GraphIndex` is built for 500k+ nodes); this change bounds what
  crosses the wire and what the browser holds, not server RAM.
