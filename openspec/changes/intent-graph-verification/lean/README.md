# Formal model of the intent-graph-verification proposal (Lean 4)

The certified core of the intent checker: a typed code graph, a four-pattern
decidable spec logic whose verifier *is* the decision procedure
(`check := decide`), change-impact analysis (touched / impact / frontier /
obligations), the discharge discipline and the admissibility gate.
Dependency-free Lean 4 (core only, no Mathlib).

> **Status: compiles (task 1.1 done).** `lake build` green on Lean 4.30.0
> with `sorry` only on the four named obligations O1–O4. The only adjustment
> needed from the first draft: the `prefix` binder renamed `pre` (Lean
> keyword). All toy-instance `example`s are kernel-accepted by `decide` —
> including the rejection of the schema-field-without-contract-field graph
> and the admissibility of the corrected change — and every `#eval` matches
> its predicted value (touched-set order aside). Four proof obligations
> remain `sorry`-marked by design and are closed by task 1.2. The change is
> complete only when `lake build` is green with **zero `sorry`**, per the
> `spec-graph-verification` precedent.

```bash
lake build                              # with a Lean 4 toolchain (≥ 4.29)
nix shell nixpkgs#lean4 -c lake build   # or via nix
```

## Guarantee → theorem map

| Guarantee | Theorem | Status |
|---|---|---|
| **The verifier is sound** — a `true` verdict means the spec holds in the model | `check_sound` (one line: `of_decide_eq_true`, because `sat` is decidable by bounded quantification) | stated, proof term supplied |
| **The verifier is complete** — a holding spec is reported `true` | `check_complete` (`decide_eq_true`) | stated, proof term supplied |
| The omission fixture is decided by the kernel — schema field without contract field is rejected, corrected change accepted | `example : ¬ sat G₁ specSchemaContract := by decide` and the `admissible cB … = true` example (section 5) | stated, kernel-checked once it compiles |
| (O1) Computed reachability implies a real path | `reach_sound` | **open (`sorry`)** |
| (O2) Fuel \|V\| suffices, under well-formedness (edge endpoints declared) — the theorem that dictates the exporter contract | `reach_complete` | **open (`sorry`)** |
| (O3) The impact set is closed under reverse dependency | `impact_closed` | **open (`sorry`)** |
| (O4) The `stableSignature` discharge rule preserves observable behaviour (reachable signature/label pairs) — the safety argument required of every rule | `stableSignature_safe` | **open (`sorry`)** |

## What the model abstracts

- **Extraction fidelity**: the theorems hold over whatever projection was
  exported; whether the projection reflects the codebase is the
  `extraction-fidelity-harness`'s territory.
- **Intent → spec fidelity**: deliberately outside — validation, not
  verification. Only `nontrivial` (a spec accepts some probe and rejects
  some probe) is decidable and modeled.
- **Rule safety beyond the graph**: O4's "observable behaviour" is the
  graph-level definition (reachable signature/label pairs); runtime
  behavioural equality is out of model, which is why acks exist.
