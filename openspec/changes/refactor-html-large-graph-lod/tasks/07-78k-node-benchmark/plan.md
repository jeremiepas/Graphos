<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — 78K-node benchmark on solario — PLAN

**Task slug**: `07-78k-node-benchmark`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Run the full pipeline on the solario codebase to produce a 158K-node graph, verify all new fields are populated correctly, and measure performance in the browser via `graphos serve`. This is the final Check gate for the entire change.

## Detail

### Scope

**Execution:**
1. Regenerate the solario graph: `cabal run graphos -- <solario-path>` (or use the existing `../../solario/graphos-out/graph.json` if regeneration is too slow, but it must go through the new pipeline to get `community_id` + aggregates + `graph.sqlite`)
2. Serve via `graphos serve --dir graphos-out --port 8080`
3. Open `http://localhost:8080/graph.html` in a browser and verify

**Verification (automated where possible, manual where not):**

1. `community_id` population:
   - Python script to parse `graph.json`: count nodes with non-null `community_id` → must equal total node count (158,166)
   - Verify `community_aggregates` length == 17,651 (actual community count)

2. `graph.sqlite` validation:
   - `sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM nodes;"` → must equal 158,166
   - `sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM edges;"` → must equal 184,616
   - `sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM community_edge_pairs;"` → must be > 0

3. Browser performance (DevTools):
   - Initial overview load < 3 seconds (DevTools Performance tab)
   - Drill-down into a community < 500ms (DevTools Performance tab)
   - Pan/zoom > 30fps with `hideEdgesOnMove` enabled (DevTools Performance)
   - Browser tab memory < 200 MB (DevTools Memory)

4. COOP/COEP headers:
   - `curl -I http://localhost:8080/graph.html` → must show both headers

5. No-regression:
   - `graph.json` node count, edge count, community count unchanged vs. baseline `../../solario/graphos-out/graph.json`

### Check Criteria

**Tests/gates:**
- (a) `graph.json` has 158,166 nodes, all with non-null `community_id` → PASS/FAIL
- (b) `community_aggregates` has 17,651 entries → PASS/FAIL
- (c) `graph.sqlite` exists with ~66 MB, correct row counts → PASS/FAIL
- (d) Initial overview load < 3s → PASS/FAIL
- (e) Drill-down < 500ms → PASS/FAIL
- (f) Pan/zoom > 30fps → PASS/FAIL
- (g) Browser tab memory < 200 MB → PASS/FAIL
- (h) Node/edge/community counts match baseline → PASS/FAIL
- (i) COOP/COEP headers present → PASS/FAIL

**Spec scenarios satisfied:**
- `html-lod-viewer/spec.md` — Scenario "Overview phase renders community dots only": 17,651 dots, 0 individual node dots
- `html-lod-viewer/spec.md` — Scenario "Drill-down expands a single community": members rendered with internal + bridge edges
- `html-lod-viewer/spec.md` — Scenario "No simultaneous full-graph render": node-level dots < total node count
- `html-lod-viewer/spec.md` — Scenario "Initial load under 3 seconds": overview interactive within 3s
- `html-lod-viewer/spec.md` — Scenario "Drill-down under 500ms": expansion completes within 500ms
- `html-lod-viewer/spec.md` — Scenario "Pan/zoom stays above 30fps": frame rate > 30fps
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all 158,166 nodes have correct `community_id`

**PASS conditions:**
- (a)–(i) all PASS

**FAIL boundaries:**
- (a) Any node in a community has null `community_id` → FAIL
- (b) `community_aggregates` length != 17,651 → FAIL
- (c) `graph.sqlite` missing or wrong row counts → FAIL
- (d) Initial load > 3s → FAIL (performance target)
- (e) Drill-down > 500ms → FAIL (performance target)
- (f) Pan/zoom < 30fps → FAIL (performance target)
- (g) Memory > 200 MB → FAIL (performance target)
- (h) Counts differ from baseline → FAIL (regression)
- (i) COOP/COEP headers missing → FAIL (OPFS won't work)

### Affected Modules

None (validation only). This task exercises the output of Tasks 1–6.

### Prerequisites

- Tasks 1–6 completed and passing
- Solario test data available at `../../solario/`
- Browser with DevTools (Chrome/Edge recommended for Performance/Memory tabs)

### Risks

- **Regeneration time**: Running `graphos` on the full solario path may take > 5 minutes on 158K nodes. If so, use the existing `../../solario/graphos-out/graph.json` but run a minimal "re-export" through the new pipeline.
- **Browser memory**: At 158K nodes, browser memory is the primary concern. The revised architecture (SQLite + sigma.js) targets < 200 MB, but this must be measured. If it fails, investigate JS DataSet retention or SQLite page cache behavior.
- **OPFS availability**: If the test browser doesn't support OPFS or isn't served with COOP/COEP headers, the SQLite drill-down won't work. Ensure `graphos serve` is running with the correct headers.
- **sigma.js performance**: The original design assumed sigma.js v3 would handle 17.6K community dots fluidly. If it doesn't, the next PDCA cycle evaluates WebGL instanced rendering or coarser clustering.
