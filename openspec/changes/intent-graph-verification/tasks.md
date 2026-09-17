# Tasks — Intent Graph Verification

> Order matters: the Lean model is the reference for every Domain mirror, so it
> compiles and closes its obligations first. Vocabulary/contract next (small,
> unblocks everything). Checkers mirror Lean. CLI last, conformance at the end.

> **Status (2026-09-17)** — task 1.1 done at proposal time: `lake build` green
> on Lean 4.30.0, `sorry` only on O1–O4, all toy examples kernel-accepted
> (`by decide`), every `#eval` matching its prediction. Sole fix needed:
> `prefix` binder renamed `pre` (Lean keyword).

## 1. Formal model (first — it is the reference for the Domain checkers)

### 1.1 Compile the skeleton
- [x] Check criteria: `lake build` green on `lean/` (Lean 4 core, no Mathlib), `sorry`
      allowed only on the four named obligations O1–O4; all toy-instance `example`s
      (`by decide`) accepted by the kernel — including the kernel-level rejection of the
      schema-field-without-contract-field graph
- [x] Fix expected skeleton issues: only `prefix` → `pre` was needed (Lean keyword);
      decidability instances elaborated as written; `lake-manifest.json` generated

### 1.2 Close the proof obligations
- [ ] O1 `reach_sound`: fuel-bounded `reach` implies an inductive `Path`
- [ ] O2 `reach_complete`: fuel |V| saturates, under well-formedness (edge endpoints
      declared) — this theorem *is* the justification for the exporter contract
- [ ] O3 `impact_closed`: the computed impact set is closed under reverse dependency
- [ ] O4 `stableSignature_safe`: the shipped discharge rule preserves observable
      behaviour (reachable signature/label pairs) — the safety argument Decision 3 requires
- [ ] Check criteria: `lake build` green, zero `sorry`, theorem map in `lean/README.md`
      updated from "obligation" to "proved"

## 2. Vocabulary + contract (small, unblocks everything)

### 2.1 Relation extension
- [ ] Check criteria first: contract delta scenarios (round-trip; tolerant load by a
      pre-change consumer path; cypher + studio read the new relations intact)
- [ ] Extend `Relation` with `tests` / `maps` + `relationToText`/`textToRelation` +
      Hspec round-trip cases

### 2.2 Node body hash
- [ ] Check criteria first: optional-field tolerance scenario (old graphs load; old
      consumers ignore the field); hash stability (same span bytes ⇒ same hash)
- [ ] Optional `body_hash` on `Node` (SHA256 of the symbol's source span, reusing the
      existing FileSystem cache hashing) + JSON round-trip Hspec

## 3. Intent code-graph projection (depends on 2)

### 3.1 Deterministic kind classifier
- [ ] Check criteria first: classification is pure, config-driven (globs, LSP symbol
      kind, name patterns), stable across runs; no rule matched ⇒ kind `other`; never a
      model call
- [ ] Pure Domain classifier + config schema; Hspec on a fixture tree covering every
      intent kind

### 3.2 Projection + well-formedness
- [ ] Check criteria first: every edge endpoint declared in the projection (O2's
      hypothesis); `NodeId` preserved verbatim (µ = identity); signature and body hash
      present for code nodes where LSP provided them; loader rejects ill-formed input
      naming the dangling endpoint
- [ ] UseCase projection from the built graph; Infrastructure export/load; Hspec
      well-formedness property test (QuickCheck)

## 4. Domain checkers mirroring Lean (depends on 1, 3)

### 4.1 `Domain/IntentCheck.hs`
- [ ] Check criteria first: on the Lean toy fixtures, Haskell verdicts are identical to
      the kernel-accepted ones for all four patterns and `and`/`not`; property test that
      `reaches`/`covered` agree with FGL reachability
- [ ] Bounded-quantifier evaluation of the four patterns; fixture parity Hspec

### 4.2 `Domain/Impact.hs`
- [ ] Check criteria first: touched = removed ∪ added ∪ changed (attributes *or*
      outgoing edges — extends `Domain/Graph/Diff.hs`, which today only yields
      added/removed); impact closed under reverse dependency (mirror of O3, property
      test); frontier = impact \ touched; `stableSignature` discharges the body-only
      toy change and does NOT discharge the signature-change variant
- [ ] touched/impact/frontier/discharge/openObligations/complete/admissible; fixture
      parity with the Lean toy instance (changes A and B)

## 5. CLI (depends on 4)

### 5.1 `graphos intentcheck`
- [ ] Check criteria first: violated spec ⇒ non-zero exit naming the spec and the
      witnessing nodes; all pass ⇒ zero; `--probe` graphs trigger non-triviality
      warnings without gating; single-document JSON with `--json`
- [ ] `graphos intentcheck --specs FILE [--graph G] [--probe G]... [--json]` —
      registered with helper + example per `cli-subcommand-help`; markdown report

### 5.2 `graphos impact`
- [ ] Check criteria first: obligations listed with per-node discharge status
      (ack / rule name / open); `--gate` exits non-zero unless admissible; acks are
      recorded in the report (audit trail)
- [ ] `graphos impact --before G --after G' [--specs FILE] [--new-spec NAME]
      [--ack ID]... [--json] [--gate]` — registered with helper + example

## 6. Conformance (last)

### 6.1 Self-check and validation
- [ ] Synthetic before/after pair from the toy instance run through `graphos impact
      --gate`: change A (schema field without contract field) rejected, change B
      admitted
- [ ] `cabal build` + `cabal test` green; `cd lean && lake build` green, zero `sorry`,
      in CI beside them
- [ ] `openspec validate intent-graph-verification` passes; tick every scenario in the
      PR description
