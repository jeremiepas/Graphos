# Implementation Plan: Unified View State and Depth Selector (Task 5)

## Implementation Steps

### 1. Define Viewer State and Dispatcher
- In the JS section of `HTML.hs`, define a `ViewerState` interface/type.
- Implement a `viewerState` singleton object.
- Implement a `dispatch(action, payload)` function to manage all state transitions.
- Actions: `SET_DEPTH`, `SET_SELECTION`, `SET_HOPS`, `SET_FACETS`, `SET_SEARCH`, `CLEAR_SELECTION`.

### 2. Implement Depth Control UI
- Create the HTML structure for the depth selector (e.g., a button group or select menu).
- Add event listeners to the dispatcher for depth changes.
- Ensure the UI reflects the current state.

### 3. Implement Renderer Lifecycle Management
- Modify the rendering logic to:
    - Check if a vis-network instance already exists.
    - If yes, call `network.destroy()` and clean up the canvas before creating a new one.
    - This ensures exactly one canvas exists per depth switch.

### 4. Implement Custom Depth (N-hop BFS)
- Add a client-side BFS function that uses the existing in-memory graph data (the interned tables) to find N-hop neighbors.
- Implement the `Custom` depth UI to allow selecting N (1–6).
- Add a warning if the resulting node set exceeds 2,000.

### 5. Implement Persistence
- Add `saveState()` and `loadState()` functions using `window.sessionStorage`.
- `saveState()`: Serialize the state object to JSON.
- `loadState()`: Parse JSON from `sessionStorage`, validate against the schema, and fall back to `Overview` if invalid.

### 6. Cleanup
- Remove all references to `btnBack` in the HTML, CSS, and JS.
- Remove the `currentPhase` and `expandedCommunity` variables.

## Implementation Details
- **N-hop BFS**: Will traverse the `_edgesData` (the interned triples) to find neighbors.
- **State Persistence**: Use a key like `graphos_viewer_state`.
- **Renderer Destruction**: Use `vis.Network.prototype.destroy`.
