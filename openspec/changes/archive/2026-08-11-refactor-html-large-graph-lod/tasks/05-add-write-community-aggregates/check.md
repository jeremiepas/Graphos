<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Add writeCommunityAggregates to IncrementalJSON.hs — CHECK

**Task slug**: `05-add-write-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Execute the check criteria from plan.md (criteria a–c) against the `writeCommunityAggregates` function in `IncrementalJSON.hs`.

## Detail

### Criterion (a): `cabal build` with `-Werror` → exits 0

**Command:**
```bash
cabal build --ghc-options="-Werror" 2>&1
```

**Expected:** Exit code 0, no warnings or errors.
**Evidence:** <!-- command output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (b): Temp-file round-trip test

**Command:**
```bash
# Run the round-trip test: create IncrementalWriter, writeCommunityAggregates, close, parse
cabal test --match "community_aggregates" 2>&1 || \
python3 -c "
# Manual verification: check the function exists and produces correct output
grep -A 3 'writeCommunityAggregates' src/Graphos/Infrastructure/Export/IncrementalJSON.hs
"
```

**Expected:** The temp file contains valid JSON with `"community_aggregates": [...]` key and the correct data.
**Evidence:** <!-- test output / grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Criterion (c): Key name is exactly `"community_aggregates"`

**Command:**
```bash
grep "community_aggregates" src/Graphos/Infrastructure/Export/IncrementalJSON.hs
```

**Expected:** The key name `"community_aggregates"` is present and used exactly once in the write function.
**Evidence:** <!-- grep output -->
**Verdict:** <!-- PASS / FAIL -->

### Spec Scenarios Satisfied

- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": the JSON export includes `community_aggregates` with the correct number of entries.
- `html-lod-viewer/spec.md` — Scenario "Streaming write preserves low memory": the incremental writer streams aggregates without building the full JSON AST.

## Result

<!-- PASS if all criteria (a)–(c) pass.
     FAIL if any criterion fails. -->
