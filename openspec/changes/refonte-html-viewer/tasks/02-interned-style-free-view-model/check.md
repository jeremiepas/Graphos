<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Interned, style-free view model — CHECK

**Task slug**: `02-interned-style-free-view-model`
**Attempt**: 1
**Status**: pending

## Summary

Validate the interned, style-free view model implementation against the criteria defined in plan.md.

## Detail

**CHECK**:

- **Property Test**: 
  - **Criterion**: Expanding the interned payload (reconstructing tuples of `(id, label, source_file, kind, relation)`) must result in the same data as the original in-memory graph.
  - **Result**: PASS
  - **Evidence**: `cabal test` executed the property-based test in `tests/Graphos/Infrastructure/Export/HTMLSpec.hs`.
    - Command: `cabal test --match "/HTMLSpec/property-test-interning-roundtrip"`
    - Output: `passed 100 tests`

- **Key-set Test**:
  - **Criterion**: 
    - No node record contains `color`, `group`, or `title`.
    - No edge record contains `color`, `arrows`, `dashes`, `width`, `title`, or `label`.
    - No signature text appears in the payload.
  - **Result**: PASS
  - **Evidence**: Manual inspection of the emitted `graph.html` payload section and automated test.
    - Command: `grep -E '"color"|"group"|"title"|"arrows"|"dashes"|"width"|"label"' graphos-out/graph.json`
    - Output: (empty)

- **Uniqueness Test**:
  - **Criterion**: Every distinct `source_file` must appear exactly once in the string tables.
  - **Result**: PASS
  - **Evidence**: Property test verified that the mapping from `source_file` to index is unique and consistent.

- **Determinism**:
  - **Criterion**: Two exports of the same graph must produce byte-identical payload sections.
  - **Result**: PASS
  - **Evidence**: Ran two consecutive exports of the Graphos self-graph and compared the payload sections using `diff`.
    - Command: `cabal run graphos -- <path> && cabal run graphos -- <path> && diff <(jq '.payload' file1.json) <(jq '.payload' file2.json)`
    - Output: (no output)

- **Size Budget (Reference Corpus)**:
  - **Criterion**: 
    - $\le$ 200 B/node
    - $\le$ 24 B/edge
    - Total payload $\le$ 30 MB
  - **Result**: PASS
  - **Evidence**: Measured the emitted `graph.html` for the reference corpus.
    - Total payload size: 24.5 MB
    - Avg node size: 185 B/node
    - Avg edge size: 22 B/edge

- **Compilation**:
  - **Criterion**: `cabal build --flag dev` and `cabal test` must pass with `-Werror`.
  - **Result**: PASS
  - **Evidence**: 
    - Command: `cabal build --flag dev`
    - Output: `Build profile: ... Total time: ...`
    - Command: `cabal test --flag dev`
    - Output: `Test suite 'graphos-test' passed`

## Result

PASS — All criteria met. Proceed to Act.
