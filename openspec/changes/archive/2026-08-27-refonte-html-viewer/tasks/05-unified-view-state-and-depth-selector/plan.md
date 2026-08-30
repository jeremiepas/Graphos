# Plan — 5.P Unified view state and depth selector

## Scope
Refactor the viewer from a phase-based global state (`currentPhase`, `expandedCommunity`)
to a unified `ViewerState` object managed by a single dispatcher.
Add a depth selector (`Overview | Community | Full | Custom`), client-side N-hop BFS
for `Custom`, state persistence via `sessionStorage`, and remove `btnBack`.

## Check Criteria
- [ ] Four depth levels offered; default is `Overview`.
- [ ] Switching depth destroys the previous vis-network instance and leaves exactly one canvas.
- [ ] `Custom` at N=2 renders the same node set as `graphos neighbors <id> --depth 2`.
- [ ] N-hop count is clamped to 1–6; expansions over 2,000 nodes warn first.
- [ ] State (depth, selection, hops, facets) survives a page reload; stale references fall back to `Overview`.
- [ ] No `btnBack` element or event listeners remain in the DOM.

## Affected Modules
- `assets/viewer/viewer.js`
- `assets/viewer/viewer.css`
- `src/Graphos/Infrastructure/Export/HTML.hs` (if new payload fields are needed)

## Risks
- Single dispatcher could become complex or buggy.
- Client-side BFS on large graphs may hang (mitigation: limit N, warn on >2,000 nodes).
- Malformed `sessionStorage` could crash the viewer (mitigation: safe parsing, fallback to `Overview`).
