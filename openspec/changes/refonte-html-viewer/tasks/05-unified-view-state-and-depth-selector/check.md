# Verification Plan: Unified View State and Depth Selector (Task 5)

## Manual Browser Checks

### 1. Depth Selector & Default State
- [ ] Open `graph.html` (generated from a test graph).
- [ ] Verify that the depth selector is present.
- [ ] Verify that the default depth is set to `Overview`.
- [ ] Verify that the graph renders the `Overview` level correctly.

### 2. Renderer Lifecycle
- [ ] Click through all four depth levels (`Overview` -> `Community` -> `Full` -> `Custom`).
- [ ] Verify that only one `<canvas>` element exists in the DOM at any time.
- [ ] Verify that there are no visual artifacts or overlapping canvases after switching.

### 3. Custom Depth (N-hop BFS)
- [ ] Select `Custom` depth.
- [ ] Set N = 2.
- [ ] Verify the nodes rendered match the expected 2-hop neighborhood of the center node.
- [ ] Compare the rendered node set against the CLI command: `graphos neighbors <id> --depth 2`.
- [ ] Test N = 1 and N = 6 (clamping/limit check).
- [ ] Verify a warning appears if an N-hop expansion would exceed 2,000 nodes.

### 4. Persistence
- [ ] Set depth to `Community` and select a node.
- [ ] Refresh the page.
- [ ] Verify that the depth remains `Community` and the selection is preserved.
- [ ] Manually corrupt `sessionStorage` (e.g., `sessionStorage.setItem('graphos_viewer_state', 'invalid')`) and refresh.
- [ ] Verify the viewer falls back to `Overview` without error.

### 5. Cleanup
- [ ] Verify that the `btnBack` button is no longer visible or present in the DOM.

## Automated/Semi-Automated Checks
- [ ] Run `node --check` on the emitted HTML to ensure valid JS syntax.
- [ ] (Optional) If a headless browser test is available, run a script to verify the canvas count.
