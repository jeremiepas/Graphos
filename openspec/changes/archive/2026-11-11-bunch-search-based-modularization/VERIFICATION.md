# Verification — Lean arm

Artifact: `formalization/lean/GraphosFormalization/Bunch/MQ.lean`
(namespace `GraphosFormalization.Bunch`), imported from
`formalization/lean/GraphosFormalization.lean`.

## Toolchain

- Pinned: `formalization/lean/lean-toolchain` → `leanprover/lean4:v4.14.0`.
  `elan`/`lake` read it automatically; `lean.yml` installs elan from a pinned
  release with a checksum and runs `lake build` (report-only, non-blocking).
- The module is written against Lean core only (no `Rat`, no `Mathlib`, no
  `Float` order lemmas) and uses only `unfold`, `split`, `omega`, `simp`,
  `rw`, `calc`, `decide` and `Nat.*` core lemmas, so it elaborates on newer
  releases as well. `nix run nixpkgs#lean4` currently provides 4.35.0;
  `nixpkgs#lean` provides Lean 3 and must not be used.

## Commands

Authoritative (pinned toolchain, whole companion):

```bash
cd formalization/lean
lake build
```

Expected: exits 0, zero errors, zero warnings for
`GraphosFormalization.Bunch.MQ`.

Module-only check with a core toolchain of any recent release (the companion's
other modules are pinned to 4.14.0 and do not all elaborate on 4.35):

```bash
cd formalization/lean
lean GraphosFormalization/Bunch/MQ.lean
```

Record (2026-11-10): `lean --version` → `Lean (version 4.35.0, Release)`,
binary `/nix/store/nnjcp6rrlhzpcn5lqx65gyfhjdv8ipmn-lean4-4.35.0/bin/lean`;
the module-only check exited 0 with no output (no warnings). The 4.14.0 run is
the `lake build (report-only, non-blocking)` job of `lean.yml` on this branch.

## What is checked

| Theorem | Statement | Spec scenario |
|---|---|---|
| `cfDen_pos`, `cf_le_one` | `0 < cfDen`, `cfNum ≤ cfDen` (`0 ≤ CF ≤ 1`) | ranges |
| `cf_closed` | `µ ≠ 0 ∧ ext = 0 ⇒ cfNum = cfDen` (`CF = 1`) | closed cluster |
| `cf_paper_fixture` | `cfNum 1 3 = 2 ∧ cfDen 1 3 = 5` (`2/5`) | paper fixture |
| `mqDen_pos`, `mq_le_k` | `0 < mqDen`, `mqNum ≤ k · mqDen` (`0 ≤ MQ ≤ k`) | ranges |
| `intra_le_total` | `intra ≤ total` (intra-edge fraction `≤ 1`) | all edges internal |
| `mqNum_eq_zero_of_noIntra` | no intra-edge ⇒ `mqNum = 0` | singleton partition |
| `mu_moveTo`, `ext_moveTo`, `cf_moveTo` | a move of one node changes `µ`, `ext`, `CF` only of its source and destination clusters | incremental update agrees with full recompute (design D3) |
| `twoTriangles_mq`, `twoTriangles_intra` | two triangles + bridge: `MQ = 84/49`, intra `6/7` | Haskell oracle fixture (task 0.2) |

Not formalised: hill-climbing termination (covered by the iteration cap in the
implementation), the threshold/annealing behaviour, and the landscape
statistics.
