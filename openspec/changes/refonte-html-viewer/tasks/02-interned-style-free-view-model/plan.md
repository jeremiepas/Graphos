# Task 2 — Interned, style-free view model — PLAN

**Task slug**: `02-interned-style-free-view-model`
**Attempt**: 1
**Status**: `in-progress`

## Summary

Replace the current ad-hoc `VisNode`/`VisEdge` records with a memory-efficient view-model using string interning for repetitive fields.

## Detail

### PLAN

**Scope**: 
Refactor `Infrastructure/Export/HTML.hs:806–983` to use a projected view-model instead of raw graph data.

**Key Changes**:
- Define new view-model records for nodes and edges.
- Implement string tables for:
  - Node IDs
  - `source_file`
  - `kind`
  - `relation`
- Update edge emission to use integer indices: `[srcIdx, tgtIdx, relIdx]`.
- Remove redundant/constant fields from the payload:
  - Nodes: `color`, `group`, `title`.
  - Edges: `color`, `arrows`, `dashes`, `width`, `title`, `label`.
- Remove signature text from the payload.
- Update the embedded viewer JavaScript (as string literals in `HTML.hs`) to read the new payload shape so the HTML remains functional.

**Affected Modules**:
- `src/Graphos/Infrastructure/Export/HTML.hs`
- `tests/Graphos/Infrastructure/Export/HTMLSpec.hs`

**Prerequisites**:
- Task 1 (Baseline measurement) must be completed to verify the size win.

**Risks**:
- An interning bug could corrupt the visual representation (e.g., mapping an ID to the wrong string).
- Payload shape changes might break the viewer if the JS update is incomplete.

**Check Criteria**:
- **Property Test**: Expanding the interned payload (reconstructing tuples of `(id, label, source_file, kind, relation)`) must result in the same data as the original in-memory graph.
- **Key-set Test**: 
  - No node record contains `color`, `group`, or `title`.
  - No edge record contains `color`, `arrows`, `dashes`, `width`, `title`, or `label`.
  - No signature text appears in the payload.
- **Uniqueness Test**: Every distinct `source_file` must appear exactly once in the string tables.
- **Determinism**: Two exports of the same graph must produce byte-identical payload sections.
- **Size Budget (Reference Corpus)**: 
  - $\le$ 200 B/node
  - $\le$ 24 B/edge
  - Total payload $\le$ 30 MB
- **Compilation**: `cabal build --flag dev` and `cabal test` must pass with `-Werror`.

