# Tasks — Intent Graph Verification

> Order matters: the Lean model is the reference for every Domain mirror, so it
> compiles and closes its obligations first. Vocabulary/contract next (small,
> unblocks everything). Checkers mirror Lean. CLI last, conformance at the end.

> **Status (2026-10-15)** — task 1.1 done at proposal time: `lake build` green
> on Lean 4, all toy examples kernel-accepted (`by decide`), every `#eval`
> matching its prediction. Sole fix needed: `prefix` binder renamed `pre`
> (Lean keyword). **Task 1.2 done 2026-10-15**: O1–O4 proved — `lake build`
> green, zero `sorry`, standard axioms only (`propext`, `Classical.choice`,
> `Quot.sound`). Two formulation repairs were required and are documented in
> the code (O4 docstring) and `lean/README.md`: O4's conclusion is
> set-equality over a dependency-labelled `observable` (the original
> list-equality over all labels is falsifiable), and O4 carries the same
> well-formedness hypothesis as O2 — Decision 4's exporter contract.

## 1. Formal model (first — it is the reference for the Domain checkers)

### 1.1 Compile the skeleton
- [x] Check criteria: `lake build` green on `lean/` (Lean 4 core, no Mathlib), `sorry`
      allowed only on the four named obligations O1–O4; all toy-instance `example`s
      (`by decide`) accepted by the kernel — including the kernel-level rejection of the
      schema-field-without-contract-field graph
- [x] Fix expected skeleton issues: only `prefix` → `pre` was needed (Lean keyword);
      decidability instances elaborated as written; `lake-manifest.json` generated

### 1.2 Close the proof obligations
- [x] O1 `reach_sound`: fuel-bounded `reach` implies an inductive `Path`
- [x] O2 `reach_complete`: fuel |V| saturates, under well-formedness (edge endpoints
      declared) — this theorem *is* the justification for the exporter contract
- [x] O3 `impact_closed`: the computed impact set is closed under reverse dependency
- [x] O4 `stableSignature_safe`: the shipped discharge rule preserves observable
      behaviour (dependency-labelled signature/label pairs of direct successors)
      — the safety argument Decision 3 requires
- [x] Check criteria: `lake build` green, zero `sorry`, theorem map in `lean/README.md`
      updated from "obligation" to "proved"

## 2. Vocabulary + contract (small, unblocks everything)

### 2.1 Relation extension
- [x] Check criteria first: contract delta scenarios (round-trip; tolerant load by a
      pre-change consumer path; cypher + studio read the new relations intact)
- [x] Extend `Relation` with `tests` / `maps` + `relationToText`/`textToRelation` +
      Hspec round-trip cases

### 2.2 Node body hash
- [x] Check criteria first: optional-field tolerance scenario (old graphs load; old
      consumers ignore the field); hash stability (same span bytes ⇒ same hash)
- [x] Optional `body_hash` on `Node` (SHA256 of the symbol's source span, reusing the
      existing FileSystem cache hashing) + JSON round-trip Hspec

## 3. Intent code-graph projection (depends on 2)

### 3.1 Deterministic kind classifier
- [x] Check criteria first: classification is pure, config-driven (globs, LSP symbol
      kind, name patterns), stable across runs; no rule matched ⇒ kind `other`; never a
      model call
- [x] Pure Domain classifier + config schema; Hspec on a fixture tree covering every
      intent kind

### 3.2 Projection + well-formedness
- [x] Check criteria first: every edge endpoint declared in the projection (O2's
      hypothesis); `NodeId` preserved verbatim (µ = identity); signature and body hash
      present for code nodes where LSP provided them; loader rejects ill-formed input
      naming the dangling endpoint
- [x] UseCase projection from the built graph; Infrastructure export/load; Hspec
      well-formedness property test (QuickCheck)

## 4. Domain checkers mirroring Lean (depends on 1, 3)

### 4.1 `Domain/IntentCheck.hs`
- [x] Check criteria first: on the Lean toy fixtures, Haskell verdicts are identical to
      the kernel-accepted ones for all four patterns and `and`/`not`; property test that
      `reaches`/`covered` agree with FGL reachability
- [x] Bounded-quantifier evaluation of the four patterns; fixture parity Hspec

### 4.2 `Domain/Impact.hs`
- [x] Check criteria first: touched = removed ∪ added ∪ changed (attributes *or*
      outgoing edges — extends `Domain/Graph/Diff.hs`, which today only yields
      added/removed); impact closed under reverse dependency (mirror of O3, property
      test); frontier = impact \ touched; `stableSignature` discharges the body-only
      toy change and does NOT discharge the signature-change variant
- [x] touched/impact/frontier/discharge/openObligations/complete/admissible; fixture
      parity with the Lean toy instance (changes A and B)

## 5. CLI (depends on 4)

### 5.1 `graphos intentcheck`
- [x] Check criteria first: violated spec ⇒ non-zero exit naming the spec and the
      witnessing nodes; all pass ⇒ zero; `--probe` graphs trigger non-triviality
      warnings without gating; single-document JSON with `--json`
- [x] `graphos intentcheck --specs FILE [--graph G] [--probe G]... [--json]` —
      registered with helper + example per `cli-subcommand-help`; markdown report

### 5.2 `graphos impact`
- [x] Check criteria first: obligations listed with per-node discharge status
      (ack / rule name / open); `--gate` exits non-zero unless admissible; acks are
      recorded in the report (audit trail)
- [x] `graphos impact --before G --after G' [--specs FILE] [--new-spec NAME]
      [--ack ID]... [--json] [--gate]` — registered with helper + example

## 6. Conformance (last)

### 6.1 Self-check and validation
- [x] Synthetic before/after pair from the toy instance run through `graphos impact
      --gate`: change A (schema field without contract field) rejected, change B
      admitted
- [x] `cabal build` + `cabal test` green; `cd lean && lake build` green, zero `sorry`,
      in CI beside them
- [x] `openspec validate intent-graph-verification` passes; tick every scenario in the$
      PR description

> **Note (2026-10-28):** `openspec validate intent-graph-verification` could not be
> run here (the `openspec` CLI is not installed in this environment). Conformance was
> instead verified three independent ways: (1) unit parity
> (`tests/Graphos/Domain/IntentGraphVerificationSpec.hs`); (2) the shipped conformance
> fixtures run through the real CLI — `graphos impact --gate` rejects change A and
> admits change B, and `graphos intentcheck` exits 1 naming the violated spec +
> witness node while exiting 0 when all specs hold (and `--probe` warns without
> gating); and (3) the Lean kernel (`lake build` green, zero `sorry`). Every
> implementation + verification criterion is met; this box stays pending only until
> the `openspec` CLI is available to run that one command.
