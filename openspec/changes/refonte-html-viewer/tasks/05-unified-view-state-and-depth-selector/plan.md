# Plan: Unified View State and Depth Selector (Task 5)

## Scope
Replace the current ad-hoc state management in the HTML viewer (`currentPhase`, `expandedCommunity` in `HTML.hs:189–190`) with a centralized `ViewerState` object.

### Key Components:
1. **State Object**: A single source of truth containing:
   - `depth`: `Overview | Community | Full | Custom`
   - `selection`: Currently selected node ID.
   - `hops`: Current N-hop count (for `Custom`).
   - `facets`: Active facet filters.
   - `searchResults`: Current search results.
2. **Dispatcher**: A function to handle state transitions and trigger side effects (like re-rendering or destroying the old renderer).
3. **Depth Control UI**: A new control surface (absorbed from `add-profondeur-view-selector`) providing the four depth levels.
4. **Custom Depth Logic**: Client-side N-hop BFS (N=1–6, default 2) to allow local exploration without server round-trips.
5. **Persistence**: Use `sessionStorage` to persist the state, with a safe fallback to `Overview` if the state becomes stale or invalid.
6. **Cleanup**: Remove the `btnBack` element and its associated handlers.

## Check Criteria
- [ ] Four depth levels are offered; `Overview` is the default.
- [ ] Switching depths results in exactly one renderer instance and one canvas on the page.
- [ ] `Custom` depth at N=2 produces the same node set as `graphos neighbors <id> --depth 2`.
- [ ] N-hop count is clamped to 1–6; expansions > 2,000 nodes trigger a warning.
- [ ] State (depth, selection, hops, facets) survives a page reload.
- [ ] No `btnBack` element or event handlers remain in the emitted HTML.

## Affected Modules
- `src/Graphos/Infrastructure/Export/HTML.hs` (the JS/CSS string literals)

## Risks
- **Renderer Leaks**: Failing to properly destroy the previous vis-network instance during depth switches could lead to memory exhaustion and multiple overlapping canvases.
- **State Corruption**: Invalid `sessionStorage` data could crash the viewer; requires robust hydration/fallback logic.
- **BFS Performance**: Client-side N-hop BFS must be efficient to avoid UI freezes on larger local subgraphs.
