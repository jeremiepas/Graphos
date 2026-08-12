# Task 2 — Refactor JS state machine to multi-depth dispatcher — PLAN

**Task slug**: `02-refactor-js-state-machine-multi-depth-dispatcher`
**Attempt**: 1
**Status**: pending

## Summary

Replace the `currentPhase: 'overview' | 'drilldown'` two-phase state with `currentDepth: 'overview' | 'community' | 'full' | 'custom'` and introduce a `switchDepth(level)` dispatcher. Factor existing overview-init code into `renderOverview()` and drilldown into `renderCommunity(cid)`. Add `destroyCurrentNetwork()` helper. Leave `renderFull` and `renderCustom` as stubs for Tasks 3 and 4.

## Detail

### Scope

- File: `src/Graphos/Infrastructure/Export/HTML.hs`
- Changes to `htmlBody`: (a) rename `currentPhase` to `currentDepth` everywhere; (b) add `function switchDepth(level)` dispatcher; (c) factor existing overview-init code into `renderOverview()`; (d) refactor drilldown code into `renderCommunity(cid)`; (e) add `function destroyCurrentNetwork()` helper; (f) wire `#depthSelector` change event to call `switchDepth()`.

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied:**
- `html-depth-selector/spec.md` — "Selector present on load and defaults to Overview": `switchDepth('overview')` routes to `renderOverview()`
- `html-depth-selector/spec.md` — "Overview selectable from any depth": `switchDepth('overview')` destroys existing network and re-renders community dots
- `html-lod-viewer/spec.md` — "Community depth expands a single community": `switchDepth('community')` routes to `renderCommunity(expandedCommunity || firstCommunityId())`
- `html-lod-viewer/spec.md` — "Swapping community within Community depth": `renderCommunity(cid)` accepts a community id parameter

**PASS conditions:**
1. Generated `graph.html` contains `let currentDepth = 'overview';` (not `currentPhase`)
2. `rg "currentPhase" graphos-out/graph.html` returns 0 (zero matches for `currentPhase`)
3. `rg "function switchDepth" graphos-out/graph.html` finds the dispatcher definition
4. `switchDepth('overview')` calls `renderOverview()` internally
5. `switchDepth('community')` calls `renderCommunity(expandedCommunity || firstCommunityId())` internally
6. `switchDepth('full')` calls stub `renderFull()`
7. `switchDepth('custom')` calls stub `renderCustom()`
8. `cabal build` exits with zero warnings
9. `cabal test` exits with code 0

**FAIL boundaries:**
- FAIL if any `currentPhase` reference survives (variable declaration, condition, or assignment)
- FAIL if `switchDepth` is not defined as a function or routes to the wrong render function
- FAIL if `destroyCurrentNetwork()` is missing or doesn't handle both `overviewNetwork`/`drilldownNetwork` states
- FAIL if `cabal build` produces any warnings
- FAIL if `cabal test` exits non-zero

### Affected modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — `htmlBody` function (embedded JavaScript)

### Prerequisites

- Task 1 (markup + CSS) is complete — `#depthSelector` element must exist in the DOM
- Base branch: two-phase `currentPhase` state machine exists in `htmlBody`

### Risks

- **Function naming conflict**: The existing `backToOverview()` and `expandCommunity()` functions must be preserved as internal helpers. Renaming or removing them would break existing callers. Mitigation: keep these functions, repurpose them as internal helpers called by `switchDepth`.
- **State variable references**: `currentPhase` may be referenced in multiple places (declarations, comparisons, conditionals). Missing any reference would leave a broken variable. Mitigation: use `rg "currentPhase" htmlBody` before and after to verify zero matches.
- **Network instance references**: `destroyCurrentNetwork()` must correctly determine whether `overviewNetwork` or `drilldownNetwork` is non-null and destroy the right one. Mitigation: inspect the existing network creation code to understand which variable holds which network.
- **Stub functions**: `renderFull` and `renderCustom` must be valid (no syntax errors) even though they do nothing. Mitigation: define them as empty function bodies that return immediately.

## Result

<!-- Pending implementation -->
