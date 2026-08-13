# Do: Unified View State and Depth Selector (Task 5)

## Implementation Plan

### 1. Refactor HTML.hs (JS/CSS)
- Define the `ViewerState` interface in the JS section.
- Implement a `dispatch` function to handle state changes:
  - `SET_DEPTH(depth)`
  - `SET_SELECTION(nodeId)`
  - `SET_HOPS(n)`
  - `SET_FACETS(facets)`
  - `SET_SEARCH_RESULTS(results)`
- Implement `render()` which:
  - Reads state from `sessionStorage` or uses default.
  - Destroys existing vis-network instance if depth changes or selection changes.
  - Creates new vis-network instance with appropriate nodes/edges.
  - Updates the UI (depth selector, detail panel, etc.).
- Implement the depth selector UI (Overview, Community, Full, Custom).
- Implement the `Custom` depth BFS logic:
  - `getNeighbors(nodeId, depth)` function.
  - Use a simple queue-based BFS on the `_nodesData`/`_edgesData` (or the interned tables).
- Implement `sessionStorage` persistence:
  - `saveState(state)`
  - `loadState()` with validation and fallback to `Overview`.
- Remove `btnBack` and its handlers.

### 2. Update CSS
- Add styles for the new depth selector.
- Ensure styles for the detail panel and facets are included (will be done in later tasks, but placeholders or basic layout might be needed).

## Deviations from Plan
(To be filled during implementation)
