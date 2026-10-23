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

> **Continuation (2026-10-14)** — task 1.2 is live but not started. Toolchain
> confirmed: `/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin/lake build`
> (exits 0, `sorry` only at lines 336/343/350/365). O1–O4 are assigned to
> `graph-theory-expert` with a precise directive (toolchain + exact edits + per-obligation
> proof strategies + acceptance: green `lake build`, zero `sorry`, README updated, toy
> `#eval`s kernel-accepted). A prior attempt stalled (plan_only, no build evidence); closure
> is being governed by `head-rnd` (`b5f51f49-4866-45c9-8904-ff3529a92a70`) with a durable
> wake path. Closing these unblocks the downstream Haskell units 2–5.

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
       (verified: `graph-omission.json` → exit 1 `Regression "schemaContract" "6"`;
       `graph-admitted.json` → exit 0; fixtures in `conformance/`)
- [ ] `cabal build` + `cabal test` green; `cd lean && lake build` green, zero `sorry`,
       in CI beside them
- [x] `openspec validate intent-graph-verification` passes; tick every scenario in the
       PR description

> **Unit 5 + conformance completion note (2026-10-16)** — CLI and conformance are
> implemented and verified. `cabal build lib:graphos exe:graphos` green under `-Werror`;
> `openspec validate intent-graph-verification` passes; `lake build` succeeds (exit 0).
> The four `sorry`s at lines 336/343/350/365 are the pre-existing, documented O1–O4
> obligations (task 1.2), not in scope here. `cabal test` is blocked by two untracked
> unit-3 spec files (`tests/Graphos/Domain/Intent/Intent/{Classifier,Projection}Spec.hs`)
> that declare module names not matching their path and import unit-3 modules
> (`Graphos.Domain.Intent.Classifier/.Projection`) which are themselves uncommitted and
> do not compile under `-Werror` (`No instance for 'Eq KindRule'`, Classifier.hs:109).
> Those are out of scope for AVI-963 (unit 5); wiring them requires closing unit 3.
