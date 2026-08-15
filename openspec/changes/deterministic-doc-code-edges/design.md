## Context

The Infer stage currently adds similarity-based edges (`inferred`) that connect
docs and code by embedding proximity, producing noisy fan-out (one code file to
many unrelated READMEs). The reliable signals — directory co-location and symbol
mentions — are not exploited, and the semantic edge filter discards the noisy
doc links, leaving no trustworthy doc↔code connections.

## Goals / Non-Goals

**Goals:**
- Deterministic, high-precision doc↔code edges.
- A distinct `documents` relation that survives the semantic filter.
- Pure, testable linking logic in Domain/UseCase.

**Non-Goals:**
- Natural-language understanding of documentation.
- Removing existing similarity inference (kept, but clearly separated).

## Decisions

- **Co-location scoped to same-or-descendant directory of the doc file.**
  - *Alternative considered:* nearest-code heuristic across the tree — rejected,
    reintroduces fan-out noise.
- **Symbol mention uses the existing symbol/definition index**; match whole-word
  identifiers of length ≥ 4 that resolve to exactly one definition, else skip.
  - *Alternative considered:* fuzzy/substring match — rejected, low precision.
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

## Migration Plan

- Additive; regenerate graph to populate `documents` edges.
- Rollback: disable the co-location/symbol passes via config.
- Verify with `cabal test` (linking suites) and a query with `edges=semantic`
  returning both doc and code nodes for a documented component.
