# Do — 5.D Unified view state and depth selector

## Implementation Plan
- [ ] Define `ViewerState` interface and `initialState()`.
- [ ] Implement `applyState(newState)`: destroy current vis-network, update internal state, trigger re-render.
- [ ] Implement `render()` dispatcher that reads `state.depth` and runs the matching view logic.
- [ ] Add depth selector markup, CSS, and click handlers (Overview, Community, Full, Custom).
- [ ] Implement N-hop BFS for `Custom` mode, using the payload graph.
- [ ] Implement `saveState()` / `loadState()` with `sessionStorage`, validation, and fallback to `Overview`.
- [ ] Remove all `btnBack` references from JS and HTML.

## Deviations from Plan
*None — implementation pending.*
