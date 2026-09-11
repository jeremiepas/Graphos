# Tasks — Progressive Graph Interface

## 1. Slice API (foundation — everything else consumes it)

### 1.1 Response conventions
- [ ] Define the shared slice-response layer in `Graphos.Infrastructure.Server` (new `SliceAPI` module): hard caps (nodes/edges per response), cursor encoding, totals, truncation flags, graph content hash on every response
- [ ] Check criteria first (Hspec): a request above the cap is clamped and marked; totals present on every page; hash changes when the shared graph is replaced
- [ ] Route registration alongside `/api/query` in Static.hs routing — same CORS/405 behavior (`query-http-port` conventions)

### 1.2 Overview aggregates endpoint
- [ ] Serve `community_aggregates` + graph totals + hash; zero node/edge objects in the response
- [ ] Hspec: aggregates-only shape; totals match the loaded graph; 8K-community fixture stays one response

### 1.3 Community slice endpoint
- [ ] Degree-descending paged members + internal edges from `GraphIndex` (community reverse index + adjacency); bridge edges as target-community references only
- [ ] Hspec: page walk over a 1,200-member fixture with size 500 yields 3 pages / 1,200 distinct nodes; hubs on page 1; cap enforcement

### 1.4 Neighborhood slice endpoint
- [ ] BFS to requested depth with node cap; community id per node; per-direction truncation flags; not-found error naming unknown ids
- [ ] Hspec: depth-2 cap-200 shape; unknown node → error, no payload

## 2. Viewer remote data layer (depends on 1)

### 2.1 Data-layer seam
- [ ] Extract the viewer's payload reads behind one interface (aggregates / community page / neighborhood) with two implementations: interned (today's embedded) and slice-backed fetch keyed by graph hash
- [ ] Check: embedded mode byte-identical behavior on the reference corpus (existing HTMLSpec scenarios stay green)

### 2.2 Remote boot + drill-down
- [ ] Boot from the aggregates endpoint alone; drill-down fetches community pages hubs-first, renders as pages arrive with "N of M loaded" affordance fetching the next page
- [ ] Deep-link restore (viewer-navigation) fetches the addressed slice directly
- [ ] Check: remote boot issues no full-graph request (network log assertion on the synthetic large corpus); deep link loads only the addressed community

### 2.3 Memory budget + eviction
- [ ] Slice-granular LRU with protection set (current community, selection, path highlight, pinned); evicted communities re-collapse to aggregate dots; refetch on return
- [ ] Check: four 2,000-node communities under a 5,000 budget → residency within budget, current community intact, protected nodes survive

### 2.4 Load-state + hash mismatch
- [ ] In-flight indicators, aggregate-only vs resident styling, error toast with retry; on hash mismatch drop caches, reload aggregates, notify
- [ ] Check: mid-session graph replacement scenario

## 3. Export mode selection (depends on 2.1; parallel-safe with 2.2–2.4)

### 3.1 Threshold + flag in HTML export
- [ ] Payload-size threshold in `Graphos.Infrastructure.Export.HTML` (CLI flag forces either mode); above threshold emit assets-inline/no-payload document with the "requires graphos serve" fallback message; preserve streaming-to-handle writes
- [ ] Hspec (HTMLSpec): below → payload embedded + works file://; above → no payload marker present, no external origins in either mode; measure and record the default threshold in design.md

## 4. Query groups (depends on 1; parallel-safe with 2–3)

### 4.1 Group evaluation endpoint
- [ ] Evaluate a query via `queryGraphWithIndexScored`; return paged member ids + total count + per-community counts under slice conventions
- [ ] Hspec: counts over the whole graph regardless of what any client loaded; paging + truncation; read-only (no graph mutation)

### 4.2 Groups panel + coloring
- [ ] Panel CRUD (name, query, color, order, hide/isolate); first-match-wins coloring over community colors; overview dots indicate group presence per community; membership joins against resident slices
- [ ] Check: precedence reorder scenario; isolate ∧ facet conjunction; counts shown for unloaded regions

### 4.3 Persistence + import/export
- [ ] localStorage keyed by graph hash; JSON export/import of the group set
- [ ] Check: reload round-trip; cross-browser import reproduces panel order and colors

## 5. Live tuning panel (depends on 2.1; parallel-safe with 3–4)

### 5.1 Tuning state + physics/visual controls
- [ ] Single tuning-state object; physics params (gravity, link distance, repulsion, on/off) via live options update — no re-mount; visual params (degree scaling, label threshold, edge opacity, arrows) restyle datasets in place
- [ ] Check: slider scenarios (layout adjusts, selection survives); in-place restyle on a rendered drill-down

### 5.2 Budget controls + persistence
- [ ] Slice page size (client-side, ≤ server cap), resident budget (lowering triggers eviction), LOD cap; defaults table = today's compiled constants; one-click reset; localStorage by graph hash, loaded before first render
- [ ] Check: next-fetch page size scenario; budget-lowering eviction; reload keeps tuning; reset restores documented defaults

## 6. Integration + conformance (last)

### 6.1 Synthetic large-corpus run
- [ ] Generate a corpus exceeding the response caps; walk: remote boot → drill-down paging → eviction → group over unloaded regions → tuning under load; record timings against the `html-lod-viewer` latency targets
- [ ] `cabal build` and `cabal test` green

### 6.2 Spec conformance pass
- [ ] Walk every scenario across the five delta specs; tick in the PR description
- [ ] `openspec validate progressive-graph-interface` passes
