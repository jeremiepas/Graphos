# Formal validation of the spec-graph-verification proposal (Lean 4)

The trust chain's deterministic components as proved Lean: the model only
extracts, exact algorithms verify, the model adjudicates pairs — and **no stage
can exceed the authority of the one before it**. Dependency-free Lean 4; a
green `lake build` (no `sorry`, no axioms) is the validation.

```bash
lake build                              # with a Lean 4 toolchain (≥ 4.29)
nix shell nixpkgs#lean4 -c lake build   # or via nix
```

## Guarantee → theorem map

| Guarantee | Theorem | Module |
|---|---|---|
| **A valid topological order certifies acyclicity** — the gate trusts the linear-time re-checker, never the sort | `topo_certifies_acyclic` / `no_closed_chain_of_respects` (positions strictly increase along any chain — `chain_pos_lt`, `step_pos_lt` — so a closed chain forces `pos n < pos n`) | `Topo.lean` |
| An accepted acyclicity verdict rules out every closed walk | `checkAcyclic_no_cycle` (+ `checkAcyclic_sound`: verdicts are certificate-checked by construction) | `Topo.lean` |
| **Path certificates are sound** — an "implemented" verdict's witness is a genuine chain from the requirement to a code node | `findPathTo_certified` (BFS output re-checked by `isPathTo` before acceptance) | `Paths.lean` |
| Unimplemented findings and implemented witnesses are mutually exclusive | `unimplemented_no_witness` | `Paths.lean` |
| **Candidate surfacing is complete** for the shared-`constrains`-target pattern — the graph can miss nothing matching it | `areCandidates_complete` (+ `mem_constrainsTargets`) | `Candidates.lean` |
| **The gate blocks only on surfaced candidates** — the model cannot invent findings | `blocking_subset` | `Candidates.lean` |
| Every blocking pair was actually confirmed | `blocking_confirmed` | `Candidates.lean` |
| No candidates / all dismissed / all unadjudicated ⇒ nothing blocks | `no_candidates_no_block`, `all_dismissed_no_block` (+ Scenario examples) | `Candidates.lean`, `Scenarios.lean` |

`Scenarios.lean` validates all spec scenarios executably (15 examples) on a
fixture mirroring the historical real conflict: two requirements constraining
`graph.html` (self-contained vs remote mode), a 3-node dependency cycle with
its closed-walk witness, an implemented requirement with a checked path, an
unimplemented one, a stale reference to a superseded ADR, and the gate under
confirming / dismissing / failing adjudicators.

## What the model abstracts

- **Extraction fidelity**: the theorems hold over whatever graph was
  extracted; extraction completeness is the harness's territory
  (`extraction-fidelity-harness`), which is why negative coverage findings
  warn instead of gating.
- **The adjudicator**: modeled as an arbitrary function `pair → verdict` —
  the theorems hold for *any* model, good or bad; that is the point.
- **Kahn's algorithm**: deliberately unverified — `checkAcyclic` re-checks its
  output, so sort bugs can only cause false "cycle" alarms, never false
  acyclicity certificates.
