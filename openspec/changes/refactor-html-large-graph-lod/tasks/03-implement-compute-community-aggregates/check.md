<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Implement computeCommunityAggregates UseCase function — CHECK

**Task slug**: `03-implement-compute-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–f) against the implementation and tests for `computeCommunityAggregates`.

## Detail

### Criterion (a): `cabal build` with `-Werror` → exits 0

**Command:**
```bash
cabal build --ghc-options="-Werror" 2>&1
```

**Expected:** Exit code 0, no warnings or errors.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): `cabal test --match "aggregates"` → all tests PASS

**Command:**
```bash
cabal test --match "aggregates" 2>&1
```

**Expected:** All Hspec tests pass — inter-community edge pairs as list of (target, count) pairs, isolated community has empty inter_community_edges.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): QuickCheck property → holds for 100 test cases

**Command:**
```bash
cabal test --quickcheck "aggregate" 2>&1
```

**Expected:** QuickCheck property `prop_aggregate_count` holds for 100 test cases.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (d): No IO in the function body

**Command:**
```bash
grep -A 50 "computeCommunityAggregates" src/Graphos/UseCase/Cluster.hs | grep -c "IO"
```

**Expected:** Must be 0 within the function scope (grep the function scope, not the whole file).
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (e): No Infrastructure imports

**Command:**
```bash
grep "Infrastructure" src/Graphos/UseCase/Cluster.hs
```

**Expected:** Must be 0 (no Infrastructure imports).
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (f): Verify `caInterCommunityEdges` returns a list, not a scalar

**Command:**
```bash
grep "caInterCommunityEdges" src/Graphos/UseCase/Cluster.hs
```

**Expected:** Must show `Map.toList` (not `Map.size`), returning `[(CommunityId, Int)]`.
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `html-lod-viewer/spec.md` — Scenario "Aggregate fields populated": community with 17 members → `member_count = 17`, `bridge_count` matches articulation points, `cohesion` from Leiden, `color` from palette, `label` from labeling or fallback.
- `html-lod-viewer/spec.md` — Scenario "Inter-community edges listed": community A has 5 edges to B and 2 edges to C → `inter_community_edges` contains `[{"target": <B>, "count": 5}, {"target": <C>, "count": 2}]`.
- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": pipeline produces exactly N aggregates for N communities.

## Result

<!-- PASS if all criteria (a)–(f) pass.
     FAIL if any criterion fails. -->
