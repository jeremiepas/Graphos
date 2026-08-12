<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Interned, style-free view model — DO

**Task slug**: `02-interned-style-free-view-model`
**Attempt**: 1
**Status**: in-progress

## Summary

Implement the interned, style-free view model to reduce the size of the HTML payload.

## Detail

**DO**:

- **View-model Implementation**:
  - Defined new `VisNode` and `VisEdge` records in `src/Graphos/Infrastructure/Export/HTML.hs` that use integer indices for connections and separate string tables for repetitive metadata.
  - Refactored the projection logic to populate these string tables during the export process.
- **String Interning**:
  - Created string tables for `node_id`, `source_file`, `kind`, and `relation`.
  - Implemented efficient lookup and deduplication for these fields.
- **Edge Emission**:
  - Updated edge emission to use the format `[srcIdx, tgtIdx, relIdx]`, significantly reducing the byte count per edge.
- **Payload Minimization**:
  - Removed `color`, `group`, and `title` from node records.
  - Removed `color`, `arrows`, `dashes`, `width`, `title`, and `label` from edge records.
  - Stripped all signature text from the JSON payload.
- **Viewer Update**:
  - Updated the embedded JavaScript in `HTML.hs` to correctly interpret the new interned payload structure, ensuring the graph still renders correctly.
- **Testing**:
  - Added property-based tests to verify that the interned payload can be reconstructed into the original graph data without loss.
  - Added key-set tests to ensure no redundant fields (like `color` or `title`) are present in the emitted JSON.
  - Verified that each `source_file` is uniquely represented in the string tables.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
