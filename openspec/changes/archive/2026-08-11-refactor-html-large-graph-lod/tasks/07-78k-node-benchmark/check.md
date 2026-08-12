<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — 78K-node benchmark on solario — CHECK

**Task slug**: `07-78k-node-benchmark`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–i) against the full pipeline output on the solario codebase.

## Detail

### Criterion (a): `graph.json` has 158,166 nodes, all with non-null `community_id`

**Command:**
```bash
python3 -c "
import json
g = json.load(open('graphos-out/graph.json'))
nodes = g['nodes']
total = len(nodes)
non_null = sum(1 for n in nodes if n.get('community_id') is not None)
print(f'Total nodes: {total}')
print(f'Non-null community_id: {non_null}')
print(f'All populated: {non_null == total}')
assert total == 158166, f'Expected 158166 nodes, got {total}'
assert non_null == total, f'Expected {total} non-null, got {non_null}'
"
```

**Expected:** 158,166 nodes, all with non-null `community_id`.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): `community_aggregates` has 17,651 entries

**Command:**
```bash
python3 -c "
import json
g = json.load(open('graphos-out/graph.json'))
agg = g.get('community_aggregates', [])
print(f'Community aggregates: {len(agg)}')
assert len(agg) == 17651, f'Expected 17651 aggregates, got {len(agg)}'
"
```

**Expected:** Exactly 17,651 community aggregates.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): `graph.sqlite` exists with ~66 MB, correct row counts

**Command:**
```bash
sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM nodes; SELECT COUNT(*) FROM edges; SELECT COUNT(*) FROM community_edge_pairs;"
ls -lh graphos-out/graph.sqlite
```

**Expected:**
- nodes: 158,166
- edges: 184,616
- community_edge_pairs: > 0
- File size ~66 MB
**Evidence:** <!-- sqlite3 + ls output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (d): Initial overview load < 3s

**Command:** <!-- Manual measurement via DevTools Performance tab -->
```bash
graphos serve --dir graphos-out --port 8080 &
# Open http://localhost:8080/graph.html in Chrome DevTools
# Record initial overview load time
kill %1 2>/dev/null
```

**Expected:** Initial overview load < 3 seconds.
**Evidence:** <!-- DevTools measurement -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (e): Drill-down < 500ms

**Command:** <!-- Manual measurement via DevTools Performance tab -->
```bash
graphos serve --dir graphos-out --port 8080 &
# Open http://localhost:8080/graph.html, click a community
# Record drill-down latency
kill %1 2>/dev/null
```

**Expected:** Drill-down into a community < 500ms.
**Evidence:** <!-- DevTools measurement -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (f): Pan/zoom > 30fps

**Command:** <!-- Manual measurement via DevTools Performance tab -->
```bash
graphos serve --dir graphos-out --port 8080 &
# Open graph.html, pan/zoom with hideEdgesOnMove enabled
# Record frame rate
kill %1 2>/dev/null
```

**Expected:** Pan/zoom > 30fps with `hideEdgesOnMove` enabled.
**Evidence:** <!-- DevTools measurement -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (g): Browser tab memory < 200 MB

**Command:** <!-- Manual measurement via DevTools Memory tab -->
```bash
graphos serve --dir graphos-out --port 8080 &
# Open graph.html, check DevTools Memory tab
# Record tab memory usage
kill %1 2>/dev/null
```

**Expected:** Browser tab memory < 200 MB.
**Evidence:** <!-- DevTools measurement -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (h): Node/edge/community counts match baseline

**Command:**
```bash
# Compare against baseline
python3 -c "
import json
current = json.load(open('graphos-out/graph.json'))
baseline = json.load(open('../../solario/graphos-out/graph.json'))
print(f'Current nodes: {len(current[\"nodes\"])} (baseline: {len(baseline[\"nodes\"])})')
print(f'Current edges: {len(current[\"edges\"])} (baseline: {len(baseline[\"edges\"])})')
print(f'Current agg: {len(current[\"community_aggregates\"])} (baseline communities: {len(baseline.get(\"community_aggregates\", []))})')
assert len(current['nodes']) == len(baseline['nodes']), 'Node count mismatch'
assert len(current['edges']) == len(baseline['edges']), 'Edge count mismatch'
"
```

**Expected:** Counts match baseline (no regression).
**Evidence:** <!-- comparison output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (i): COOP/COEP headers present

**Command:**
```bash
graphos serve --dir graphos-out --port 8080 &
sleep 2
curl -I http://localhost:8080/graph.html 2>&1 | grep -iE "(Cross-Origin|COOP|COEP)"
kill %1 2>/dev/null
```

**Expected:** Both `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp` headers present.
**Evidence:** <!-- curl output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `html-lod-viewer/spec.md` — Scenario "Overview phase renders community dots only": 17,651 dots, 0 individual node dots.
- `html-lod-viewer/spec.md` — Scenario "Drill-down expands a single community": members rendered with internal + bridge edges.
- `html-lod-viewer/spec.md` — Scenario "No simultaneous full-graph render": node-level dots < total node count.
- `html-lod-viewer/spec.md` — Scenario "Initial load under 3 seconds": overview interactive within 3s.
- `html-lod-viewer/spec.md` — Scenario "Drill-down under 500ms": expansion completes within 500ms.
- `html-lod-viewer/spec.md` — Scenario "Pan/zoom stays above 30fps": frame rate > 30fps.
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all 158,166 nodes have correct `community_id`.

## Result

<!-- PASS if all criteria (a)–(i) pass.
     FAIL if any criterion fails. -->
