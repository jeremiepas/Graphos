---
description: "Task 3 — HTML viewer composition badge"
---
---
description: "Plan: add composition badge rendering in HTML viewer for community dots and drill-down headers"
---

# Task 3 — HTML viewer composition badge — PLAN

**Task slug**: `03-html-viewer-composition-badge`
**Attempt**: 1
**Status**: completed

## Summary

Add JavaScript badge rendering to `graph.html` that displays composition data (`🔧 N / 📄 M / 🌉 K`) on community dots in overview mode tooltips and on drill-down headers. Badge must be absent on legacy graphs (no `compositions` in embedded JSON).

## Detail

### Scope

- **HTML embedding**: The `Graph` JSON (including `compositions`) is already embedded in the HTML page as part of the viewer payload. The badge reads this embedded data.
- **JavaScript changes** in `src/Graphos/Infrastructure/Export/HTML.hs` (template):
  - `compositionBadge(comp)`: returns `🔧 N / 📄 M / 🌉 K` string from a `CommunityComposition` object
  - **Overview mode**: Add badge to community dot tooltips via vis-network `title` field
  - **Drill-down mode**: Add badge as static text next to community label
  - **Legacy fallback**: When `compositions` is `undefined` or missing in embedded JSON, omit badge entirely (no error, no placeholder)

### Spec Scenarios (from specs/cluster-composition/spec.md)

| Spec ID | Scenario | Task Coverage |
|---|---|---|
| SC-3.1 | Badge on community dot (overview mode) | Tooltip shows `🔧 12 / 📄 4 / 🌉 3` for community 483 |
| SC-3.2 | Badge on drill-down header | Header shows badge next to community label |
| SC-3.3 | Legacy graph omits badge | No badge rendered; no error logged |

### Check Criteria (defined BEFORE code)

**Manual verification steps:**
```bash
# Build and serve
cabal build
cabal run graphos -- serve <path-to-graph-dir>
# Then in browser:
# 1. Open graph.html for a graph WITH compositions
# 2. Hover over community dots → badge visible
# 3. Drill into community → badge in header
# 4. Open graph.html for a LEGACY graph (no compositions key)
# 5. Verify no badges, no errors in console
```

**Spec scenario gates:**

| ID | Test name pattern | PASS condition | FAIL condition |
|---|---|---|---|
| SC-3.1 | (manual) Badge visible on dot tooltip | Hover on community dot shows `🔧 N / 📄 M / 🌉 K` | Tooltip missing badge or shows garbled text |
| SC-3.2 | (manual) Badge on drill-down | Community header includes badge text | Badge missing from header |
| SC-3.3 | (manual) Legacy omits badge | No badge element rendered; browser console has zero errors | Badge shown or console error |

**Automated gate (if feasible):**
- Extract the HTML output and use `grep`/`python` to verify `compositionBadge` function exists in the HTML
- Verify that `compositions` data from JSON is passed to the JavaScript scope
- PASS: function present in HTML, compositions data accessible in JS scope
- FAIL: function absent or compositions not passed to JS

**Exact FAIL boundaries:**
- If badge renders on legacy graphs (when `compositions` is undefined) → JS error or empty badge → FAIL
- If badge text uses wrong emoji or format → visual mismatch → FAIL
- If badge breaks vis-network tooltip rendering (truncates or overlaps) → visual defect → FAIL
- If HTML output has syntax errors preventing page load → critical FAIL

### Affected Modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — template changes (JavaScript embedding)

### Prerequisites

- Task 2 must be complete: `compositions` key persisted in `graph.json` and loaded into `Graph`
- `Graph` embedded in HTML via existing export mechanism already includes `compositions` field
- vis-network tooltip API understood (uses `title` field for hover text)

### Risks

- **Risk**: HTML template is a single embedded string in Haskell code. Adding JavaScript increases template size but shouldn't affect build.
- **Risk**: vis-network `title` field may have character limits or special character issues with emoji. Test with actual emoji rendering.
- **Medium risk**: Drill-down view structure must be understood — badge placement depends on existing DOM/template structure.
- **Low risk**: Fallback (no compositions → no badge) is a simple conditional in JS.

## Result

All subtasks completed. `formatCompositionBadge` function added to `assets/viewer/viewer.js` — renders `🔧 N / 📄 M / 🌉 K`. `.legend-badge` CSS class added to `assets/viewer/viewer.css`. Badge conditionally rendered when compositions data is present; omitted on legacy graphs. Build passes with `-Wall -Werror`. Tests pass (633 examples, 0 failures).
