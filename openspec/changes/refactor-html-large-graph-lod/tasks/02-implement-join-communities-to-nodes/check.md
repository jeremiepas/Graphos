<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement joinCommunitiesToNodes UseCase function — CHECK

**Task slug**: `02-implement-join-communities-to-nodes`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–e) against the implementation and tests for `joinCommunitiesToNodes`.

## Detail

### Criterion (a): `cabal build` with `-Werror` → exits 0

**Command:**
```bash
cabal build --ghc-options="-Werror" 2>&1
```

**Expected:** Exit code 0, no warnings or errors.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): `cabal test --match "joinCommunities"` → all tests PASS

**Command:**
```bash
cabal test --match "joinCommunities" 2>&1
```

**Expected:** All Hspec tests pass — community join sets correct `community_id`, empty graph returns empty graph, multiple communities assign correctly.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): `cabal test --quickcheck "joinCommunities"` → property holds

**Command:**
```bash
cabal test --quickcheck "joinCommunities" 2>&1
```

**Expected:** QuickCheck property `prop_joinCommunities_count` holds for 100 test cases.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (d): No `IO` in the function

**Command:**
```bash
grep "IO" src/Graphos/UseCase/Cluster.hs | grep -c joinCommunities
```

**Expected:** Must be 0 (no IO in the function body or type signature).
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (e): No IO imports that could indicate accidental side effects

**Command:**
```bash
grep -c "import.*IO" src/Graphos/UseCase/Cluster.hs
```

**Expected:** Should be 0 (or only expected imports like `Data.IORef` if already present).
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `node-schema/spec.md` — Scenario "Community ID populated after Leiden": node `n1` assigned to community `4` → `community_id` in JSON is `4` (not `null`).
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all nodes in communities have non-null `community_id`.
- `node-schema/spec.md` — Scenario "Nodes outside any community remain null": isolated nodes stay `Nothing`.

## Result

<!-- PASS if all criteria (a)–(e) pass.
     FAIL if any criterion fails. -->
