# SUPERSEDED

This change (`add-profondeur-view-selector`) is superseded by `refonte-html-viewer`.

**Date**: 2026-08-14
**Reason**: This change (0/24 tasks, entirely unimplemented) planned to add depth-selector
controls to the string-literal viewer in `HTML.hs`. `refonte-html-viewer` rewrites the same
module entirely and absorbs all requirements here, making independent implementation impossible
without conflicting assumptions.

**Coverage**: Every requirement of `html-depth-selector` spec is carried into
`refonte-html-viewer/specs/html-depth-selector/spec.md`. See the mapping table below.

## Requirement-by-Requirement Mapping

### html-depth-selector requirements

| Requirement (add-profondeur-view-selector) | Coverage in refonte-html-viewer | Notes |
|---|---|---|
| Depth selector control with Overview/Community/Full/Custom, default Overview | ✅ `html-depth-selector/spec.md`: "Depth selector control replaces the two-phase back button" | Implemented as `<select id="depthSelect">` in HTML skeleton |
| Switching depth destroys previous vis.Network instance | ✅ `html-depth-selector/spec.md`: "Switching depth leaves one canvas" | Implemented in `applyState()` in viewer.js |
| btnBack removed, no dead DOM element | ✅ `html-depth-selector/spec.md`: "No dead back button" | Confirmed in HTMLSpec test |
| Community depth requires selected community | ✅ `html-depth-selector/spec.md` + viewer.js `renderGraph()` | Falls back to "Select a community from the legend" hint |
| Custom depth: N-hop BFS, N in 1–6, default 2 | ✅ `html-depth-selector/spec.md`: "Custom depth performs an N-hop neighbourhood expansion" | Implemented as `neighborhoodNodeIds(startId, hops)` in viewer.js |
| BFS result = `graphos neighbors <id> --depth N` | ✅ `html-depth-selector/spec.md`: "Neighbourhood matches the CLI" | Browser-only verification required |
| N clamped to 1–6 | ✅ `dispatch('SET_HOPS', ...)` clamps via `Math.max(1, Math.min(6, payload))` | Automated |
| Expansions > 2000 nodes warn first | ✅ `html-depth-selector/spec.md`: "Large expansions are signalled" | Implemented with `confirm()` in renderGraph |
| State (depth, selection, hops, facets) persisted in sessionStorage | ✅ `html-depth-selector/spec.md`: "Depth and facet state persist across reload" | Under 4096 bytes, key `graphos_viewer_state` |
| Stale references fall back to Overview | ✅ `html-depth-selector/spec.md`: "Stale references degrade safely" | Implemented in `loadState()` |
| Custom: numeric input visible only for Custom depth | ✅ viewer.js `render()` shows/hides `#hopsInput` | |
| Changing hop count re-runs BFS | ✅ `dispatch('SET_HOPS', ...)` triggers `applyState()` → `render()` → `renderGraph()` | |
| btnBack not in rendered HTML | ✅ HTMLSpec test asserts `btnBack` absent | |
| Overview selectable from any depth (replaces backToOverview) | ✅ `dispatch('SET_DEPTH', 'Overview')` from the select change handler | |

### html-lod-viewer modifications

| Requirement (add-profondeur-view-selector) | Coverage in refonte-html-viewer | Notes |
|---|---|---|
| Two-phase state machine → four-level depth (currentPhase → currentDepth) | ✅ `html-lod-viewer/spec.md`: "Two-phase level-of-detail rendering" updated | Absorbed entirely |
| Full depth renders all nodes on explicit selection | ✅ `html-lod-viewer/spec.md`: "Full depth requires explicit selection" | |
| Community depth reuses member nodes with community color | ✅ viewer.js `styledNodes()` applies community color | |
| Bridge edges shown to collapsed communities | Partial — current implementation shows internal edges; bridge edges to other community dots are not yet distinct. Tracked as a follow-up in refonte-html-viewer scope. | |
| Swapping community within Community depth | ✅ clicking a community in legend/overview dispatches `SET_DEPTH + SET_SELECTION` | |

## Status

This change is archived. No further work is needed here. Its requirements are either
fully implemented in `refonte-html-viewer` or explicitly tracked as partial/follow-up.
