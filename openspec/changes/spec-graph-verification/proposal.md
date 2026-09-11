# spec-graph-verification

## Why

Specs, ADRs and PRDs rot in ways code cannot: requirement A quietly contradicts
requirement B three files away, a superseded ADR keeps being cited, a SHALL never gets an
implementation, two teams write the same requirement twice in different words. Today
nothing checks any of this — this repo found a real instance by hand (`html-lod-viewer`'s
unconditional self-contained rule vs `progressive-graph-interface`'s remote mode, caught
only because one author happened to touch both).

Graphos is uniquely placed to be the checker: it already extracts docs into the graph
(concepts + relationships via LLM), extracts code via LSP, runs local small models
(`llamacpp-model-provider`), has embeddings, communities, articulation points and cypher.
What's missing is the discipline that makes verification *trustworthy with a small model*:

> **The model only extracts; deterministic graph algorithms verify.** A 7–27B model is
> never asked "is this spec corpus consistent?" — it emits typed nodes and edges from one
> document at a time (validated by the `extraction-fidelity-harness`), exact graph theory
> finds structural problems (cycles, unreachability, shared-target contradictions, stale
> supersession), and the model returns only for *pairwise* adjudication of the candidates
> the graph surfaces — a two-document question a small model answers well.

Verdicts that gate CI additionally carry **certificates**: an acyclicity verdict ships a
topological order, an implemented-requirement verdict ships the path — both re-checkable
in linear time, so the gate's trust rests on a checker small enough to certify in Lean
(this change ships that model in `lean/`, like `viewer-navigation` did).

## What Changes

- A **spec-artifact schema**: node kinds `Requirement`, `Decision`, `Constraint`,
  `Scenario` and edge relations `refines`, `conflicts_with`, `satisfies`, `supersedes`,
  `constrains` — populated by hybrid extraction: deterministic parsing of openspec/ADR
  structure (headers, SHALL clauses, scenario blocks, ADR status lines) plus small-model
  extraction for semantic edges only.
- A **`graphos speccheck` command** running the deterministic check catalogue: dependency
  cycles (with topological-order certificates), unimplemented requirements / unspecified
  code (reachability, with path certificates on the positive side), contradiction
  candidates (shared-`constrains`-target pairs), stale supersession, duplication
  candidates (embedding similarity within communities), single-point-of-failure decisions
  (articulation points). Markdown + JSON report; non-zero exit on confirmed findings — a
  type-checker for specs, CI-gateable.
- A **pairwise adjudication stage**: each candidate pair goes to the configured local
  model as a two-document structured-output question; only confirmed conflicts block,
  unconfirmed candidates stay in the report. No adjudication call ever sees more than the
  two artifacts.
- A **Lean model** (`lean/` in this change) proving the checker's core guarantees:
  a valid topological order certifies acyclicity (closed-walk contradiction), path
  certificates are sound by construction, contradiction-candidate enumeration is complete
  for shared-target pairs, and the adjudication gate can only block on surfaced
  candidates.

## Capabilities

### New Capabilities

- `spec-artifact-schema`: the typed spec-graph vocabulary and its hybrid
  (deterministic + small-model) extraction with harness validation.
- `spec-graph-checks`: the deterministic, certificate-carrying check catalogue and the
  `graphos speccheck` CLI with report formats and exit-code gating.
- `conflict-adjudication`: bounded pairwise small-model adjudication of graph-surfaced
  candidates with confirmed-blocks / candidate-reports discipline.

### Modified Capabilities

- `graph-json-contract`: gains the spec-artifact relation vocabulary (`refines`,
  `conflicts_with`, `satisfies`, `supersedes`, `constrains`) as first-class edge
  relations alongside the existing eight, tolerated by all existing consumers.

## Impact

- **Code**: Domain (`Types/Edge.hs` relation vocabulary; new pure `Domain/SpecCheck.hs` —
  cycles/reachability/pair enumeration mirrored by the Lean model), UseCase
  (`SpecExtract` hybrid parse, `SpecCheck` orchestration, adjudication prompts),
  Infrastructure (openspec/ADR structural parser, report writer, reuse of the LLM client
  + embeddings), CLI (`speccheck` command, registered with helper per
  `cli-subcommand-help`).
- **APIs**: none — CLI only.
- **Dependencies**: none new; the local model rides the existing OpenAI-compatible
  provider config.
- **Formal**: `lean/` model built in CI-checkable form (`lake build`, no sorry); the
  Domain checker mirrors it clause for clause.
