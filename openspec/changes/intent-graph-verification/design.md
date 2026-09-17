# Design — Intent Graph Verification

## Context

Verify code against formalized intent, and turn a change into a finite obligation list —
with the same trust discipline as `spec-graph-verification`: deterministic algorithms
carry all authority, the Lean model in `lean/` is the reference, and no model sits in
the checked path. Layering: spec semantics, impact closure and discharge are pure Domain
(`Domain/IntentCheck.hs`, `Domain/Impact.hs`); projection, check and impact orchestration
are UseCase; spec-file/config loading and reports are Infrastructure; `intentcheck` and
`impact` are CLI.

## Decision 1 — A decidable spec logic; the verifier is the decision procedure

All four spec patterns quantify only over `G.nodes`, so satisfaction is decidable and
the checker is literally `decide` — soundness of the verifier becomes a one-line theorem
(`check_sound`) instead of a program-verification effort. The Domain evaluator mirrors
the Lean semantics clause for clause and is tested against the same fixtures.

| Alternative | Rejected because |
|---|---|
| Embed a Datalog engine | New dependency, and the certification story shifts from "one `decide`" to trusting an external evaluator; the four patterns cover the observed omission classes without it |
| Regular path queries (full RPQ language) | More expressive, but not needed by any motivating case; can be added later as new `Spec` constructors without changing the architecture |
| Ask an LLM whether the intent is implemented | Unauditable, unreproducible, and exactly the failure mode this change exists to remove; models return only where they are strong — per-node verdicts on an enumerated frontier, outside the gate |

## Decision 2 — Four patterns chosen from real omission classes

`reaches` (a named node of kind k reaches kind k′ via label ℓ — "the handler calls a
validator"), `absent` (no node of a kind with a name prefix, modulo whitelist — migration
completion), `covered` (every node of kind k is reached by kind k′ — "every handler is
tested"), `parallel` (total two-way correspondence between two kinds via a label —
schema ↔ contract ↔ ORM ↔ front coherence, the single most valuable omission detector).
`and`/`not` compose them.

| Alternative | Rejected because |
|---|---|
| Full first-order logic over the graph | Decidability and predictable cost are lost; every motivating example fits the four patterns |
| Temporal logic (CTL/LTL) | There is no execution-time dimension here; graph reachability already captures the needed transitivity |
| Bijective `parallel` (strict one-to-one) | Real correspondences are sometimes one-to-many (one schema field, several projections); totality both ways is the invariant that catches omissions — uniqueness ships later as a refinement flag |

## Decision 3 — Impact as reverse closure; frontier as obligations; discharge is ack ∨ safety-obligated rule

Touched `T` comes from the before/after diff (removed, added, changed in attributes *or
outgoing edges* — `Domain/Graph/Diff.hs` provides added/removed today and gains the
changed-detection). Impact is the reflexive-transitive closure of `T` under reversed
dependency edges (`calls`, `implements`, `references`, `maps`, `depends_on`). The
frontier `F = I \ T` — impacted but unmodified — is exactly the omission surface, and it
is *finite and enumerable*: review stops being "did we forget anything?" and becomes
"rule on these N nodes". An obligation is discharged only by an explicit ack (audited)
or by a registered rule; every rule must carry a safety argument — for the shipped
`stableSignature` rule (all touched direct dependencies kept signature, kind and name;
only bodies changed), the argument is theorem O4: the node's observable behaviour
(reachable signatures per label) is unchanged.

| Alternative | Rejected because |
|---|---|
| Diff-only review | Misses omissions by construction — the motivating failure |
| Re-check all specs, no frontier | The admissibility gate catches spec-covered omissions, but gives the reviewer no per-node worklist for everything specs don't cover yet; both are needed |
| Rules as unproven heuristics | A discharge rule silently wrong is worse than no rule — it converts an obligation into false confidence; hence one safety obligation per rule, in Lean |
| Include `tests` in the propagation labels | A changed node makes its *tests* stale, not broken — test staleness is a `covered` spec concern, not impact propagation; keeping `tests` out keeps the frontier signal tight |

## Decision 4 — Lean first, Domain mirrors; well-formedness is the exporter's contract

The Lean model is the reference implementation and ships with the change (the
`spec-graph-verification` / `viewer-navigation` pattern). `check_sound` and
`check_complete` are already theorem-complete by construction; four obligations remain
and are the roadmap: O1 `reach_sound` (fuel-bounded reachability implies a real path),
O2 `reach_complete` (fuel |V| suffices — *requires* every edge endpoint to be a declared
node), O3 `impact_closed` (the impact set is closed under reverse dependency), O4
`stableSignature_safe` (the shipped rule preserves observable behaviour). O2's
hypothesis dictates a contract: the exporter SHALL emit only edges whose endpoints are
declared nodes, and the loader SHALL verify it — well-formedness is not an optimization,
it is a proof hypothesis.

| Alternative | Rejected because |
|---|---|
| Prove the Haskell implementation directly | No Haskell verification toolchain in this stack; the mirror-plus-shared-fixtures discipline already caught divergence in `spec-graph-verification` |
| Skip O1–O4, trust the skeleton | `check_sound` only certifies the *semantics*; without O1/O2 the `reach` computation inside the semantics could be wrong and soundly checked against the wrong meaning |
| Mathlib for closure lemmas | Core-only Lean keeps the CI toolchain footprint identical to the existing `lean/` precedent |

## Decision 5 — Deterministic kind classification; µ is identity on stable ids

`nodeKind` today is free-text from LSP symbol kinds. The intent kinds (handler,
validator, test, schema-field, contract-field, doc, module) are assigned by
deterministic, config-declared rules (path globs, LSP symbol kind, name patterns,
annotations) — never by a model — so checker verdicts are stable across runs and
findings are reproducible. Change correspondence µ is the identity on `NodeId`, which is
already deterministic (file + symbol derived); the before/after exports must preserve
it, which the projection contract states as a requirement.

| Alternative | Rejected because |
|---|---|
| LLM-assigned kinds | Node identity and classification must be deterministic for verdicts to be stable — same reason `spec-graph-verification` parses structure deterministically |
| Name-based node matching for µ | Fragile under rename; renames should surface as remove+add (both touched), which is the honest impact |
| Hardcoded classification heuristics | Projects disagree on layout; config rules keep the classifier pure and testable while staying project-adaptable |

## Deferred

- **Intent → spec formalization and its fidelity measurement** (scenario-based
  agreement with the requester, spec→NL back-translation, per-sentence traceability with
  visible unformalized residue, mutation testing against empty specs, calibration on
  validated-then-reworked history). Probabilistic by nature — validation, not
  verification. Only the decidable fragment ships now: the `nontrivial` probe.
- **Evolutionary-coupling edges from git history** (files that change together) to
  complement extractor-known dependencies — a candidate `co_changed` relation for a
  later contract delta.
- **Bijective `parallel`** and RPQ-style path patterns as `Spec` extensions.

## Verification strategy

`cabal build` and `cabal test` green (Hspec fixtures shared with the Lean toy instance,
identical verdicts asserted); `cd lean && lake build` green with **no `sorry`** at
change completion; `graphos impact` self-run on a synthetic before/after pair derived
from the toy instance; `openspec validate intent-graph-verification` passes.
