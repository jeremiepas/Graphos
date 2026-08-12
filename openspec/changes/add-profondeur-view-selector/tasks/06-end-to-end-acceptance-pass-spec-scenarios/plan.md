# Task 6 — End-to-end acceptance pass against the spec scenarios — PLAN

**Task slug**: `06-end-to-end-acceptance-pass-spec-scenarios`
**Attempt**: 1
**Status**: pending

## Summary

Run the full manual acceptance checklist against the regenerated `graph.html` on the `example/` graph, covering every scenario in `specs/html-depth-selector/spec.md` and the MODIFIED scenarios in `specs/html-lod-viewer/spec.md`. No new code unless a scenario fails.

## Detail

### Scope

- No code changes unless a scenario fails. This is the validation gate.
- Actions: (a) regenerate `graph.html` via `cabal run graphos -- example/`; (b) serve via `graphos serve --dir graphos-out --port 8080`; (c) walk through each criterion in a browser; (d) for criterion 5, run CLI `cabal run graphos -- neighbors <id> --depth 2` and diff against browser render.

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied (verbatim from specs):**
- `html-depth-selector/spec.md` — "Selector present on load and defaults to Overview"
- `html-depth-selector/spec.md` — "Switching to Full re-renders all nodes"
- `html-depth-selector/spec.md` — "No overlapping canvases on depth switch"
- `html-depth-selector/spec.md` — "Community depth expands last selected community"
- `html-depth-selector/spec.md` — "No community selected defaults to community 0"
- `html-depth-selector/spec.md` — "Custom depth shows neighborhood input"
- `html-depth-selector/spec.md` — "N=2 neighborhood matches CLI neighbors"
- `html-depth-selector/spec.md` — "Changing hop count re-runs BFS"
- `html-depth-selector/spec.md` — "Reload preserves selected depth"
- `html-depth-selector/spec.md` — "Reload preserves Custom neighborhood parameters"
- `html-depth-selector/spec.md` — "Absent keys default to Overview"
- `html-depth-selector/spec.md` — "No back button in the DOM"
- `html-depth-selector/spec.md` — "Overview selectable from any depth"
- `html-lod-viewer/spec.md` — "Overview depth renders community dots only"
- `html-lod-viewer/spec.md` — "Community depth expands a single community"
- `html-lod-viewer/spec.md` — "Full depth renders all nodes on explicit selection"
- `html-lod-viewer/spec.md` — "No simultaneous full-graph render unless explicitly selected"
- `html-lod-viewer/spec.md` — "Member nodes colored by community"
- `html-lod-viewer/spec.md` — "Bridge edges shown to collapsed communities"
- `html-lod-viewer/spec.md` — "Swapping community within Community depth"

**Acceptance criteria (10 criteria from tasks.md):**
1. Selector present on load, defaults to Overview
2. Switching to Full on < 1K-node graph renders all nodes within 1s
3. Switching to Community expands last-selected (or community 0)
4. Custom depth shows neighborhood input (min 1, max 6, value 2)
5. N=2 neighborhood matches `graphos neighbors <id> --depth 2`
6. Changing hops re-runs BFS
7. Reload preserves depth via `sessionStorage`
8. No `btnBack` element in DOM
9. Overview selectable from any depth
10. `cabal build` zero warnings + `cabal test` green

**PASS conditions:**
- All 10 criteria PASS
- All spec scenarios (19 total) PASS
- No regression on existing LOD viewer behavior (overview renders community dots, drill-down expands a community, self-contained HTML)

**FAIL boundaries:**
- FAIL if any single criterion from the 10-item checklist does not PASS
- FAIL if any spec scenario does not PASS (the scenario conditions are not met)
- FAIL if `cabal build` produces warnings (even if manual scenarios pass)
- FAIL if `cabal test` exits non-zero
- FAIL if an existing LOD viewer regression is found (e.g., overview no longer renders community dots, drill-down no longer works)

### Affected modules

- None (validation only). If failures are found, the fix goes back into the relevant prior task's Do step.

### Prerequisites

- Tasks 1–5 are complete and passing their individual checks
- Example graph data available at `example/` directory

### Risks

- **Cross-browser inconsistencies**: sessionStorage behavior varies (Safari private mode throws, Firefox may clear on restart). Mitigation: the try/catch in persistDepth handles throws; verify on Chrome and Firefox at minimum.
- **Manual verification subjectivity**: "Renders all nodes within 1s" is a subjective timing check. Mitigation: use a stopwatch/timer in the browser and record the actual time. If it's close to 1s (e.g., 1.5s) on the example graph, it's acceptable — the spec says < 1K nodes which is a small graph.
- **BFS CLI vs. browser mismatch**: The CLI `graphos neighbors` and the client-side BFS must produce the same node set. If they differ, the BFS implementation (Task 4) needs fixing. Mitigation: this is expected to surface edge-direction bugs — record the diff if found and start Task 4 attempt 2.

## Result

<!-- Pending implementation -->
