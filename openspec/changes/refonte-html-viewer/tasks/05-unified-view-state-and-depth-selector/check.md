# Check: Unified View State and Depth Selector (Task 5)

## Verification Plan

### 1. Manual Browser Verification
- **Depth Levels**: Open `graph.html` and verify that:
  - `Overview` is the default.
  - Four levels (`Overview`, `Community`, `Full`, `Custom`) are selectable.
  - Switching between them works.
- **Renderer Integrity**: During depth switching, inspect the DOM to ensure:
  - The old `<div class="vis-network">` (or equivalent) is removed/cleaned up.
  - Only one canvas exists at a time.
- **Custom Depth (BFS)**:
  - Select `Custom` and set `N=2`.
  - Compare the visible nodes with the output of `graphos neighbors <id> --depth 2`.
  - Verify N is clamped to 1–6.
  - Verify warning for > 2,000 nodes.
- **Persistence**:
  - Set a depth and a selection.
  - Reload the page.
  - Verify the depth and selection are restored.
  - Manually corrupt `sessionStorage` (e.g., `sessionStorage.setItem('viewer_state', 'invalid')`) and verify it falls back to `Overview`.
- **Cleanup**:
  - Verify no `btnBack` element is present in the HTML.

### 2. Automated Verification
- **JS Syntax**: Run `node --check` on the extracted/emitted JS.
- **Build**: `cabal build --flag dev` to ensure no Haskell errors.
- **Tests**: (If applicable) Run existing tests to ensure no regressions.

## Results
(To be filled during verification)
- **Four depth levels offered**: [ ]
- **One renderer instance/canvas**: [ ]
- **Custom depth N=2 matches CLI**: [ ]
- **N clamped to 1–6 / Warning for >2k**: [ ]
- **State survives reload**: [ ]
- **No btnBack remains**: [ ]

### Verification Log
(Record findings here)
