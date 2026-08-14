# CONTEXT: CSS Implementation for Search and Depth Selector

## 🎯 Objective
Implement specific CSS rules to complete the visual requirements for:
1. **Task 04**: Search result elements (`.search-verdict`, `.search-suggestions`, `.result-item.scored`).
2. **Task 05**: The new Depth Selector UI component.

## 🛠 Technical Context

### 1. Search UI (Task 04)
- **Current State**: CSS has been migrated from `HTML.hs` to `assets/viewer/viewer.css`.
- **Target Elements**:
    - `.search-verdict`: Displayed when an API query returns a verdict.
    - `.search-suggestions`: Displayed when API suggests alternative queries.
    - `.result-item.scored`: Result items returned by the API that include a relevance score.
- **Existing Styles**: `.result-item` has basic styling. `.h1result`, `.h2result`, and `.docresult` handle color coding via `border-left-color`.
- **Required Work**: Define typography, spacing, and visual distinction for the verdict, suggestions, and the "scored" state.

### 2. Depth Selector UI (Task 05)
- **Requirement**: A UI component to switch between `Overview`, `Community`, `Full`, and `Custom` modes.
- **Current HTML**: The `<header>` in `src/Graphos/Infrastructure/Export/HTML.hs` only contains the title and `.search-box`.
- **Target Work**: 
    - **HTML**: Add the selector element to the header in `HTML.hs`.
    - **CSS**: Add styling for the selector (e.g., segmented control or buttons) in `assets/viewer/viewer.css`.
    - **JS**: Ensure it triggers `dispatch('SET_DEPTH', ...)` in `assets/viewer/viewer.js`.

## 📂 Files to Modify
- `assets/viewer/viewer.css`: Add all new CSS rules.
- `src/Graphos/Infrastructure/Export/HTML.hs`: Add the HTML structure for the depth selector in `htmlHeader`.
- `assets/viewer/viewer.js`: (If necessary) Wire the new UI element to the `viewerState` dispatcher.

## 🎨 Design Guidelines
- **Theme**: Maintain the dark theme (Background: `#0f0f1a`, Primary: `#7dd3fc`, Secondary: `#252540`).
- **Consistency**: Use existing spacing, font sizes, and transition effects.
- **Interactivity**: Include `:hover`, `:focus`, and `.active` states for all new interactive elements.

## ✅ Verification Criteria
- [ ] Search verdict and suggestions are clearly legible and styled.
- [ ] Scored results are visually distinct from regular results.
- [ ] Depth selector is visible in the header and matches the theme.
- [ ] Switching depth via the selector updates the graph (verifies JS/CSS integration).
