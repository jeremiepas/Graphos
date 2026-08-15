## Why

Documentation is ingested (2,729 doc nodes) and linked to code (2,133 doc↔code
edges), but the links are embedding/similarity-based: a single `.rs` file was
linked to a dozen unrelated `README.md` files. With `edges=semantic` the doc
links were dropped entirely (a jwt-verifier query returned 125 code nodes and
0 doc nodes). The strongest real signals — a README living beside the code, and
a doc naming a symbol — are not used, so doc↔code matching is effectively useless.

## What Changes

- Add **co-location edges**: a doc file links to code in its own and descendant
  directories with a high-confidence `documents` relation.
- Add **symbol-mention edges**: when doc text references a known identifier, link
  the doc to the node defining that symbol.
- Tag these edges as `documents` (high confidence) to distinguish them from the
  existing similarity-based `inferred` edges.
- Keep `documents` edges in the `semantic` (non-ambiguous) edge set so they
  survive filtering.

## Capabilities

### New Capabilities
- `doc-code-linking`: deterministic documentation-to-code edges via directory
  co-location and symbol-name mention, distinct from similarity inference.

### Modified Capabilities
<!-- If an existing 'infer' or 'edges' spec exists, this adds a new edge relation;
     confirm during specs phase whether it modifies edge-classification requirements. -->

## Impact

- **UseCase/Infer** stage: add co-location and symbol-mention passes.
- **Domain/Graph**: new `documents` edge relation and confidence tagging.
- **Domain/Context / query**: `documents` edges included in semantic edge set.
- Improves doc↔code retrieval quality; no user-facing breaking change.
