# Tasks — Spec Graph Verification

> **v1 implementation status (2026-09-12)** — deterministic trust chain
> implemented and green: Relation vocabulary extended (contract delta),
> `Domain/SpecCheck.hs` mirrors the Lean model (same fixtures, identical
> verdicts asserted by `SpecCheckSpec`), openspec structural parser
> (requirements/scenarios/spans/contains + capability cross-references),
> `graphos speccheck` CLI with subcommand help, markdown/JSON reports,
> certificate-checked verdicts and gate discipline. Self-check runs on this
> repo's own corpus (541 spec nodes). 823 tests, 0 failures.
> **Not yet done**: ADR file parsing (3.1 partial — spec.md only), the
> small-model semantic edge pass (3.2), duplication/SPOF findings (4.2),
> `--check` filter and `--adjudicate` (4.3 partial, 5), self-check
> shared-target confirmation (6.1 — needs the semantic pass to emit
> constrains edges).

## 1. Formal model (first — it is the reference for the Domain checker)

### 1.1 Lean model
- [x] `lean/` project: spec-graph types, chain/walk checker, `isTopoOrder` re-checker,
  fuel-BFS with checked path certificates, shared-target pair enumeration, adjudication
  gate — with theorems `topo_certifies_acyclic` (closed-chain contradiction via strict
  index increase), chain-index monotonicity, path soundness by construction,
  `candidates_complete`, `blocking_subset`; every spec scenario as an executable example
- [x] `lake build` green, no sorry (see `lean/README.md` for the theorem map)

## 2. Vocabulary + contract (small, unblocks everything)

### 2.1 Relation extension
- [ ] Extend `Relation` with `refines`/`conflicts_with`/`satisfies`/`supersedes`/`constrains` + JSON round-trip; Hspec round-trip cases; tolerant-load check on a pre-change consumer path
- [ ] Check: contract delta scenario (cypher + studio load the new relations)

## 3. Extraction (depends on 2)

### 3.1 Deterministic structural parser
- [ ] Infrastructure parser for openspec spec.md (requirement headers, SHALL clauses, scenario blocks → Requirement/Scenario nodes with spans, `contains` edges) and ADR files (status, "Supersedes" → Decision nodes, `supersedes` edges, active flag in `extra`)
- [ ] Hspec: parse fixtures from this repo's own openspec/ specs; spans correct; no-model invariant (provider disabled ⇒ full structure)

### 3.2 Semantic edge pass
- [ ] Per-document structured-output call emitting edges between existing ids only (`refines`/`satisfies`/`constrains`/`conflicts_with`/`depends_on`); unknown-id drop + count; harness corpus entries for the extraction schema
- [ ] Check: one-document-per-call scenario; drop-counting scenario

## 4. Checks (depends on 2; Domain mirrors lean/)

### 4.1 Domain checker
- [ ] `Domain/SpecCheck.hs`: Kahn sort + `isTopoOrder` re-checker (mirrors Lean), closed-walk witness on cycle, BFS with path certificates + `isWalk` re-checker, shared-target pair enumeration, stale-supersession pattern, gate filter
- [ ] Hspec: run the Domain checker on the Lean fixtures' graphs and assert identical verdicts; property test for pair completeness

### 4.2 Analysis-backed findings
- [ ] Duplication candidates (embedding cosine within community, threshold config); SPOF decisions via articulation points restricted to Decision nodes
- [ ] Hspec: threshold behavior; SPOF on a two-community fixture

### 4.3 speccheck CLI
- [ ] `graphos speccheck [--graph G] [--json] [--check NAME]... [--strict-coverage] [--strict-duplicates] [--adjudicate]` — registered with helper + example; markdown report; single-document JSON; exit gate (cycles + confirmed conflicts; coverage/duplicates under strict flags)
- [ ] Check: clean-corpus-exits-zero, JSON-single-document, cycle-witness and certificate scenarios

## 5. Adjudication (depends on 3 + 4)

### 5.1 Pairwise calls + gate
- [ ] Prompt + verdict schema (conflict|compatible|duplicate + rationale citing both), retries, `unadjudicated` on schema failure; gate = pure filter (blocking ⊆ confirmed ⊆ candidates, mirrored in Lean)
- [ ] Hspec: gate subset property; unadjudicated non-gating; call-boundedness (two bodies per call) via a mock client
- [ ] Check: the four adjudication scenarios

## 6. Conformance (last)

### 6.1 Self-check
- [ ] Run `graphos speccheck` on this repository's own openspec/ corpus; confirm the shared-target check surfaces the historical self-contained/remote-mode constraint pair on `graph.html`
- [ ] `cabal build` + `cabal test` green; `cd lean && lake build` green in CI beside them
- [ ] `openspec validate spec-graph-verification` passes; tick every scenario in the PR description
