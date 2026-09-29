## Context

The Infer stage currently adds similarity-based edges (`inferred`) that connect
docs and code by embedding proximity, producing noisy fan-out (one code file to
many unrelated READMEs). The reliable signals — directory co-location and symbol
mentions — are not exploited, and the semantic edge filter discards the noisy
doc links, leaving no trustworthy doc↔code connections. Explicit file-path
citations in doc prose — a strong, unambiguous signal — are likewise unused.

## Goals / Non-Goals

**Goals:**
- Deterministic, high-precision doc↔code edges.
- A distinct `documents` relation that survives the semantic filter.
- Pure, testable linking logic in Domain/UseCase.

**Non-Goals:**
- Natural-language understanding of documentation.
- Removing existing similarity inference (kept, but clearly separated).

## Formal model (Lean 4)

Following the `spec-graph-verification` / `intent-graph-verification`
precedent, the deterministic passes are mirrored in a Lean 4 model
(`lean/DocLink.lean`, Lean 4 core only, no Mathlib) *before* the Domain
implementation, and the Domain passes are gated on parity with it.

- The model defines a typed `CodeGraph` (doc/code kinds, source paths, symbol
  definitions, doc text) and the three edge constructors, then proves the
  guarantee theorems: (a) `colocation_subtree` — every co-location edge stays in
  the doc's directory subtree; (b) `symbolMention_uniqueDef` — every
  symbol-mention edge targets the unique definition of a whole-word identifier
  of length ≥ 4; (c) `pathRef_resolves` — every path-reference edge resolves to
  an existing `source_file` and carries the `documents` relation; (d)
  `documents_semanticSafe` — every emitted edge has confidence ≥ 0.7, so it
  survives the semantic filter.
- Proof style is pinned by `lean-proof-methodology`: goal-guided invocation of
  cache-bearing theorems (`rw [theorem]` or defeq-forcing `have`), no `by
  decide` over `List.lookup`-bearing statements, file-wide
  `set_option linter.unusedSectionVars false`.
- `lean/VERIFICATION.md` records the toolchain (`nix run nixpkgs#lean4`; never
  `nixpkgs#lean`, which is Lean 3) and the exact `lake clean && lake build`
  command; the change is complete only when it is green with zero `sorry`.
- *Alternative considered:* prove nothing, test only in Hspec — rejected; the
  subtree guard and unique-definition rules are exactly the invariants that
  silent regressions erode, and the repo already has the Lean discipline and
  toolchain in place.

## Decisions

- **Co-location scoped to same-or-descendant directory of the doc file.**
  - *Alternative considered:* nearest-code heuristic across the tree — rejected,
    reintroduces fan-out noise.
- **Symbol mention uses the existing symbol/definition index**; match whole-word
  identifiers of length ≥ 4 that resolve to exactly one definition, else skip.
  - *Alternative considered:* fuzzy/substring match — rejected, low precision.
- **Path reference resolves explicit repository-relative paths** in doc text to
  code nodes by `source_file`, and links across subtrees (unlike co-location).
  - Requires a `/` separator, a known source extension, and resolution to an
    existing node; dangling or bare-filename tokens are skipped.
  - *Alternative considered:* co-location alone — rejected, misses cross-subtree
    citations (an ADR or `CLAUDE.md` naming a file it does not live beside).
- **New `documents` edge relation with high confidence**, added in Infer and
  included in the semantic edge set.
  - *Alternative considered:* reuse `inferred` with higher weight — rejected,
    cannot distinguish deterministic from similarity edges downstream.
- **Ambiguous symbol names (multiple definitions) are skipped**, not linked to all.
  - *Alternative considered:* link to all definitions — rejected, restores noise.

## Risks / Trade-offs

- [Common identifiers cause spurious mentions] → require length ≥ 4, whole-word,
  single-definition resolution; make thresholds configurable.
- [Monorepo README at a high directory over-links] → co-location is
  directory-scoped; a top-level README links only within its own subtree.
- [Extra edges increase graph size] → bounded by doc count and defined symbols;
  far smaller than similarity fan-out.
- [Bare filenames or non-existent paths cause spurious links] → require a path
  separator, a known source extension, and resolution to an existing
  `source_file`; unresolved tokens are skipped.

## Migration Plan

- Additive; regenerate graph to populate `documents` edges.
- Rollback: disable the co-location/symbol/path-reference passes via config.
- Verify with `cabal test` (linking suites), the Lean parity fixtures, and a
  query with `edges=semantic` returning both doc and code nodes for a
  documented component.
