# Design — Spec Graph Verification

## Context

Verify specs/ADRs/PRDs — correctness and conflict-freedom — using the graph Graphos
already builds, with only a small local model in the loop. Layering: the check algorithms
are pure Domain (`Domain/SpecCheck.hs`), extraction orchestration and adjudication
prompts are UseCase, parsers/LLM-client/report-writer are Infrastructure, `speccheck` is
CLI. The Lean model in `lean/` is the reference for the Domain checker.

## Decision 1 — The model extracts; graph theory verifies; the model adjudicates pairs

The trust chain has three stages with strictly decreasing authority:

1. **Extraction** (model, per-document, harness-validated): typed nodes/edges only.
2. **Checks** (deterministic): cycles, reachability, shared-target pairs, stale
   supersession, duplication candidates, articulation points. Complete for their
   patterns; no judgment.
3. **Adjudication** (model, per-pair): confirms or dismisses candidates; can never add
   findings the graph didn't surface.

Alternatives considered:

| Alternative | Rejected because |
|---|---|
| Ask the model to review the corpus for conflicts directly | O(n²) semantic comparisons across documents exceeds any small-model context and is unauditable; findings would be unreproducible |
| Purely structural checking, no adjudication | Shared-target candidates include many compatible pairs; without adjudication the gate is either noisy (block on candidates) or toothless (never block) |
| A large hosted model instead of small local | Works, but the design goal is that trust does NOT depend on model quality — the same architecture just gets cheaper with a small model; hosts without cloud keys (perso) still verify |

## Decision 2 — Certificates on gating verdicts, Lean on the re-checkers

A gating verdict is only accepted with a re-checkable witness: topological order for
acyclicity, explicit path for implementedness, closed walk for cycles. The re-checkers
(`isTopoOrder`, `isWalk`) are linear-time, tiny, and proved in the Lean model:

- `topo_certifies_acyclic` — a valid topological order implies no closed walk (index
  strictly increases along any chain; a closed chain would need `idx n < idx n`).
- Path-certificate soundness by construction (BFS output is re-checked before acceptance,
  the `viewer-navigation` `findPath` pattern).
- `candidates_complete` — both-constrain-same-target implies pair membership.
- `blocking_subset` — the gate blocks only on surfaced candidates.

The sort/BFS implementations may be optimized freely; only the re-checkers are trusted,
and they are the Lean-mirrored components. Alternative considered: proving the *sort*
correct instead — strictly harder for no additional trust (the certificate check is what
the gate consumes).

## Decision 3 — Hybrid extraction, deterministic-first

Openspec and ADR formats already encode most structure (requirement headers, scenario
blocks, status/supersedes lines) — parsed without a model, so node existence and spans
are never hallucinated. The model's structured output may only connect *existing* node
ids; unknown-id edges are dropped and counted. Alternative considered: full-LLM
extraction of spec docs (the current generic doc path) — rejected for this purpose
because node identity must be deterministic for findings to be stable across runs.

## Decision 4 — Negative coverage warns, positive coverage certifies

"Requirement is implemented" carries a path certificate — sound. "Requirement is
unimplemented" is a claim about the extraction's completeness, not just the graph — so it
warns by default and gates only under `--strict-coverage`. This asymmetry is deliberate
and mirrors how type systems treat inference failure vs proof.

## Decision 5 — Relation vocabulary extension over `extra` metadata

The five spec relations become first-class `Relation` values (contract delta) rather than
`extra`-bag annotations: checks key on relations via `GraphIndex` like every other
traversal, cypher can match on them, and the tolerant/strict loader split already handles
old consumers. Alternative — encoding in `extra` — rejected: invisible to the index and
to cypher `-[:constrains]->` patterns.

## Verification strategy

- **Formal model** (`lean/`): the trust-chain checkers as Lean theorems —
  `topo_certifies_acyclic`, chain-index monotonicity, path-checker soundness,
  shared-target candidate completeness, `blocking ⊆ confirmed ⊆ candidates` — plus every
  spec scenario as an executable example on fixtures. `lake build` green, no sorry, is
  the check; the Domain checker mirrors the model clause for clause.
- `cabal build` / `cabal test` — Hspec: relation round-trip, structural parser on real
  openspec files from this repo, certificate re-checkers against the Lean fixtures
  (same graphs, same verdicts), pair enumeration completeness by property test, gate
  subset property, report golden files, unknown-id drop counting.
- Scenario pass: run `speccheck` on this repository's own `openspec/` corpus — it
  contains a known historical conflict shape (self-contained vs remote-mode constraints
  on `graph.html`) that the shared-target check must surface.
