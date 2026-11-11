# Formal model of the intent-graph-verification proposal (Lean 4)

The certified core of the intent checker: a typed code graph, a four-pattern
decidable spec logic whose verifier *is* the decision procedure
(`check := decide`), change-impact analysis (touched / impact / frontier /
obligations), the discharge discipline and the admissibility gate.
Dependency-free Lean 4 (core only, no Mathlib).

> **Status: complete (tasks 1.1 + 1.2 done).** `lake build` green on Lean 4
> (core only, no Mathlib) with **zero `sorry`**: the four obligations O1–O4
> are proved (kernel-checked, standard axioms only — `propext`,
> `Classical.choice`, `Quot.sound`). The only adjustment needed from the
> first draft was the `prefix` binder renamed `pre` (Lean keyword). All
> toy-instance `example`s are kernel-accepted by `decide` — including the
> rejection of the schema-field-without-contract-field graph and the
> admissibility of the corrected change — and every `#eval` matches its
> predicted value. Per the `spec-graph-verification` precedent, the change
> is complete iff `lake build` is green with **zero `sorry`**. O2 and O4
> carry a well-formedness hypothesis (`∀ e ∈ edges, src ∈ ids ∧ dst ∈ ids`)
> — that hypothesis *is* the exporter contract (Decision 4): the exporter
> emits only edges whose endpoints are declared nodes and the loader
> verifies it. O4's conclusion is set-equality over `observable` (see its
> docstring for the two counterexamples that forced the formulation).

```bash
lake build                              # with a Lean 4 toolchain (≥ 4.29)
nix shell nixpkgs#lean4 -c lake build   # or via nix
```

## Guarantee → theorem map

| Guarantee | Theorem | Status |
|---|---|---|
| **The verifier is sound** — a `true` verdict means the spec holds in the model | `check_sound` (one line: `of_decide_eq_true`, because `sat` is decidable by bounded quantification) | proved |
| **The verifier is complete** — a holding spec is reported `true` | `check_complete` (`decide_eq_true`) | proved |
| The omission fixture is decided by the kernel — schema field without contract field is rejected, corrected change accepted | `example : ¬ sat G₁ specSchemaContract := by decide` and the `admissible cB … = true` example (section 5) | proved, kernel-checked |
| (O1) Computed reachability implies a real path | `reach_sound` | **proved** — no well-formedness needed |
| (O2) Fuel \|V\| suffices, under well-formedness (edge endpoints declared) — the theorem that dictates the exporter contract | `reach_complete` | **proved** — saturation at \|V\| via a discrete-growth pigeonhole (`ClosureToolkit`) |
| (O3) The impact set is closed under reverse dependency | `impact_closed` | **proved** — same saturation argument |
| (O4) The `stableSignature` discharge rule preserves observable behaviour (dependency-labelled signature/label pairs of direct successors) | `stableSignature_safe` | **proved** — set-equality conclusion, well-formedness of both graphs; see the O4 docstring in `IntentGraph.lean` for why the original list-equality statement is false (edge-list reordering; dangling arcs to undeclared nodes) and why non-dependency labels are excluded |

## What the model abstracts

- **Extraction fidelity**: the theorems hold over whatever projection was
  exported; whether the projection reflects the codebase is the
  `extraction-fidelity-harness`'s territory.
- **Intent → spec fidelity**: deliberately outside — validation, not
  verification. Only `nontrivial` (a spec accepts some probe and rejects
  some probe) is decidable and modeled.
- **Rule safety beyond the graph**: O4's "observable behaviour" is the
  graph-level definition (signature/label pairs over direct
  dependency-labelled successors); runtime behavioural equality is out of
  model, which is why acks exist.
