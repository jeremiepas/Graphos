<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Add CommunityAggregate Domain type — CHECK

**Task slug**: `01-add-community-aggregate-type`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–e) against the implementation in `src/Graphos/Domain/Types/Analysis.hs`.

## Detail

### Criterion (a): `cabal build` with `-Werror` → exits 0

**Command:**
```bash
cabal build --ghc-options="-Werror" 2>&1
```

**Expected:** Exit code 0, no warnings or errors.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): Hspec property — serialize `CommunityAggregate` with `caInterCommunityEdges = [(4, 5), (8, 2)]`

**Command:**
```bash
cabal test --match "inter_community_edges" 2>&1
```

**Expected:** The `inter_community_edges` field serializes to `[{"target":4,"count":5},{"target":8,"count":2}]` — an array of objects with `"target"` and `"count"` keys.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): Hspec parse — round-trip equality

**Command:**
```bash
cabal test --match "inter_community_edges.*parse\|round" 2>&1
```

**Expected:** Parsing the expected JSON shape produces the original `CommunityAggregate` value.
**Evidence:** <!-- test output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (d): Module path confirmation

**Command:**
```bash
ls src/Graphos/Domain/Types/Analysis.hs
```

**Expected:** File exists at `src/Graphos/Domain/Types/Analysis.hs` under `Domain/`.
**Evidence:** <!-- ls output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (e): Zero `IO` imports in the module

**Command:**
```bash
grep -c "IO" src/Graphos/Domain/Types/Analysis.hs
```

**Expected:** Must be 0.
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `html-lod-viewer/spec.md` — Scenario "Inter-community edges listed": `inter_community_edges` contains entries as `{"target": <cid>, "count": <n>}` objects.
- `node-schema/spec.md` — no direct impact (aggregate type, not node field).

## Result

<!-- PASS if all criteria (a)–(e) pass.
     FAIL if any criterion fails. -->
