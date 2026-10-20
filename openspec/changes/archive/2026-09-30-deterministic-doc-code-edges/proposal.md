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
- Add **path-reference edges**: when doc text cites an explicit repository-relative
  file path (e.g. `src/domain/workflow/task-definition.ts`), link the doc to the
  code nodes of that file — including across directory subtrees, which co-location
  cannot reach (an ADR or `CLAUDE.md` citing a file it does not live beside).
- Tag these edges as `documents` (high confidence) to distinguish them from the
  existing similarity-based `inferred` edges.
- Keep `documents` edges in the `semantic` (non-ambiguous) edge set so they
  survive filtering.
- Add a **Lean 4 formal model** of the three passes (`lean/DocLink.lean`,
  Lean 4 core only, no Mathlib) with kernel-checked theorem gates: co-location
  edges stay inside the doc's directory subtree, symbol-mention edges name
  identifiers with exactly one definition, path-reference edges resolve to an
  existing `source_file`, and `documents` edges carry confidence ≥ 0.7 so they
  survive the semantic filter. The methodology follows `lean-proof-methodology`
  (goal-guided invocation of cache-bearing theorems; no `by decide` over
  `List.lookup`-bearing statements); `lean/VERIFICATION.md` records the pinned
  toolchain (`nix run nixpkgs#lean4`; `nixpkgs#lean` is Lean 3 and must not be
  used) and the exact `lake clean && lake build` command, which must exit 0
  with zero errors, zero warnings, and zero `sorry`.

## Capabilities

### New Capabilities
- `doc-code-linking`: deterministic documentation-to-code edges via directory
  co-location, symbol-name mention, and explicit file-path reference, distinct
  from similarity inference.

### Modified Capabilities
<!-- If an existing 'infer' or 'edges' spec exists, this adds a new edge relation;
     confirm during specs phase whether it modifies edge-classification requirements. -->

## Impact

- **UseCase/Infer** stage: add co-location, symbol-mention, and path-reference passes.
- **Domain/Graph**: new `documents` edge relation and confidence tagging.
- **Domain/Context / query**: `documents` edges included in semantic edge set.
- **Formal**: `lean/DocLink.lean` mirrors the Domain linking logic; the Haskell
  passes must match the Lean model's verdicts on the Lean toy fixtures (same
  discipline as `intent-graph-verification`'s checker-parity gate). The change is
  complete only when `lake clean && lake build` is green with zero `sorry`.
- Improves doc↔code retrieval quality; no user-facing breaking change.
