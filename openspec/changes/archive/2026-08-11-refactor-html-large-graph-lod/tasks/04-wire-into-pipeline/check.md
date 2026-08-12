<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Wire join + aggregates into Pipeline.hs — CHECK

**Task slug**: `04-wire-into-pipeline`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–f) against the pipeline wiring, SQLite export, and COOP/COEP headers.

## Detail

### Criterion (a): `cabal build` with `-Werror` → exits 0

**Command:**
```bash
cabal build --ghc-options="-Werror" 2>&1
```

**Expected:** Exit code 0, no warnings or errors.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): `cabal test` (full suite) → all tests PASS

**Command:**
```bash
cabal test 2>&1
```

**Expected:** Full test suite passes with no failures.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): Run on small fixture → produces `graph.json` with non-null `community_id` and `community_aggregates` key

**Command:**
```bash
cabal run graphos -- tests/fixtures/small 2>&1
python3 -c "
import json
g = json.load(open('graphos-out/graph.json'))
nodes = g['nodes']
agg = g.get('community_aggregates', [])
print(f'Nodes: {len(nodes)}')
non_null = sum(1 for n in nodes if n.get(\"community_id\") is not None)
print(f'Non-null community_id: {non_null}')
print(f'Aggregates: {len(agg)}')
"
```

**Expected:** `graph.json` has non-null `community_id` on community members and `community_aggregates` key present and populated.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (d): Run on small fixture → produces `graph.sqlite` with correct row counts

**Command:**
```bash
sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM nodes; SELECT COUNT(*) FROM edges; SELECT COUNT(*) FROM community_edge_pairs;"
```

**Expected:** `graph.sqlite` exists with correct schema and row counts matching the fixture.
**Evidence:** <!-- sqlite3 output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (e): Node/edge/community counts unchanged vs. baseline

**Command:**
```bash
# Compare counts between current and baseline
python3 -c "
import json
g = json.load(open('graphos-out/graph.json'))
print(f'Nodes: {len(g[\"nodes\"])}')
print(f'Edges: {len(g[\"edges\"])}')
print(f'Communities: {len(g[\"community_aggregates\"])}')
"
```

**Expected:** Counts match baseline (no regression).
**Evidence:** <!-- comparison output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (f): COOP/COEP headers present

**Command:**
```bash
graphos serve --dir graphos-out --port 8080 &
sleep 2
curl -I http://localhost:8080/graph.html 2>&1 | grep -iE "(Cross-Origin|COOP|COEP)"
kill %1 2>/dev/null
```

**Expected:** Shows both `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp` headers.
**Evidence:** <!-- curl output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": pipeline produces exactly N aggregates for N communities.
- `html-lod-viewer/spec.md` — Scenario "Serve delivers the LOD HTML": `graphos serve --dir graphos-out --port 8080` serves the viewer.
- `node-schema/spec.md` — Scenario "Community ID populated after Leiden": `community_id` is non-null for community members.
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all nodes in communities have correct `community_id`.

## Result

<!-- PASS if all criteria (a)–(f) pass.
     FAIL if any criterion fails. -->
