# intent-graph-verification

## Why

The costliest review failures are not bugs in the diff — they are **omissions**: the
schema field added without its contract field, the handler changed without its test, the
migration that leaves three call sites on the old SDK. An omission is by definition *not
in the diff*, so diff-centred review (human or LLM) cannot see it. Asked "is anything
missing?", a model over a large change answers unreliably; asked "here are the fourteen
impacted-but-unmodified nodes, rule on each", it answers well. Nothing in Graphos today
produces that second question.

The companion change `spec-graph-verification` checks the **spec corpus** for internal
consistency. This change closes the other half of the loop: check the **code graph**
against an intent's formal spec, and turn a change (before/after graph) into a *finite,
enumerable list of review obligations*. Three regimes are kept strictly separate:

1. **Intent → formal spec** is the uncertain step (fidelity of formalization). It is
   *validation*, not verification — explicitly **out of scope** here except for the one
   decidable piece: a non-triviality probe that rejects specs which accept or reject
   everything.
2. **Spec on graph** is deterministic: the spec language has four decidable patterns
   (existence, absence, coverage, parallel-structure coherence), all quantifiers bounded
   by the node set — so the verifier *is* the decision procedure.
3. **Trust in the verifier** is a Lean theorem, proved once, independent of any instance:
   with `check := decide`, soundness (`check_sound`) is a one-line theorem. The Lean
   model ships in `lean/` (this change), following the `spec-graph-verification`
   precedent; the Domain checker mirrors it clause for clause.

Impact analysis follows the same discipline: touched nodes `T` from the before/after
diff, impact `I = D*(T)` as reverse-dependency closure, frontier `F = I \ T` as the
obligation list. An obligation is discharged only by an explicit human ack or by a
**non-affectation rule carrying its own safety obligation** (e.g. `stableSignature`:
callers of a body-only change are unaffected — safe only if observable behaviour is
provably preserved, theorem O4 in the Lean model). A change is *admissible* iff all
obligations are discharged, the new spec holds after, and every previously validated
spec still holds.

## What Changes

- An **intent code-graph projection**: the checker's input contract — stable node ids
  (µ = identity across before/after), deterministic kind classification (handler,
  validator, test, schema-field, contract-field, doc, module) from config rules with no
  model in the loop, LSP signature plus a node-level body hash (enables the
  "body changed, signature stable" rule), and well-formedness (every edge endpoint
  declared — the hypothesis `reach_complete` needs).
- An **intent-spec language and `graphos intentcheck` command**: four decidable patterns
  plus `and`/`not`, evaluated by the Domain twin of the Lean `check`; markdown + JSON
  report; non-zero exit on violated specs; non-triviality warnings on a probe set.
- A **`graphos impact` command**: touched/impact/frontier from two graphs, discharge by
  acks and registered rules (`stableSignature` shipped), open-obligations report, and
  the admissibility verdict (completeness ∧ new spec ∧ preservation of the validated
  set) as the CI-gateable exit code.
- A **Lean model** (`lean/IntentGraph.lean`): decidable semantics, `check := decide`,
  `check_sound`/`check_complete` proved; the toy instance where a schema field added
  without its contract field is rejected *by the kernel*; four named proof obligations
  (O1 `reach_sound`, O2 `reach_complete`, O3 `impact_closed`, O4
  `stableSignature_safe`) to close before the Domain mirror is trusted.

## Capabilities

### New Capabilities

- `intent-code-graph`: the typed, deterministic, well-formed graph projection consumed
  by the certified checker.
- `intent-spec-checks`: the four-pattern decidable spec language, its evaluation, the
  `intentcheck` CLI and the non-triviality probe.
- `change-impact-obligations`: touched/impact/frontier computation, the discharge
  discipline (acks + safety-obligated rules), and the admissibility gate via
  `graphos impact`.

### Modified Capabilities

- `graph-json-contract`: gains the `tests` and `maps` edge relations (test exercises
  target; parallel-structure correspondence) alongside the existing thirteen, and an
  optional node `body_hash` field — both tolerated by all existing consumers.

## Impact

- **Code**: Domain (`Types/Edge.hs` two relations; `Types/Node.hs` optional body hash;
  new pure `Domain/IntentCheck.hs` and `Domain/Impact.hs` mirroring `lean/`, the latter
  building on `Domain/Graph/Diff.hs` which today yields added/removed but not
  changed-node detection nor closure), UseCase (projection, check and impact
  orchestration), Infrastructure (spec-file loader, kind-classification config, report
  writer), CLI (`intentcheck`, `impact`, registered with helper per
  `cli-subcommand-help`).
- **APIs**: none — CLI only.
- **Dependencies**: none new. No LLM call anywhere in the checked path.
- **Formal**: `lean/` ships as a skeleton with `sorry`-marked obligations O1–O4; the
  tasks gate implementation on `lake build` green and forbid `sorry` at change
  completion, as `spec-graph-verification` did.
- **Out of scope** (future change): automatic formalization of tickets/PRDs into specs
  and its fidelity measurement (scenario-based agreement, back-translation, mutation
  testing, calibration); git evolutionary-coupling edges to complement the extractor.
