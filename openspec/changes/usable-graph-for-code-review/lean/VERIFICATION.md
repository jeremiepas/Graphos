# Verification command (tasks 1.4 / 6.1)

```bash
# Toolchain: Lean 4.29.1 core only, no Mathlib.
export PATH="/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin:$PATH"
cd openspec/changes/usable-graph-for-code-review/lean
lake clean && lake build
# Exit 0, zero errors, zero warnings, zero sorry.
```

The toolchain comes from `nix run nixpkgs#lean4` (package `lean4`,
version 4.29.1). `nixpkgs#lean` must NOT be used: it provides Lean 3.
A Lean 4.30.0 is also on this machine's PATH; the artifact is pinned to 4.29.1
like every other change artifact, so always export the store path above first.

Run log (2026-10-08):

```
$ lean --version
Lean (version 4.29.1, commit v4.29.1, Release)

$ lake clean && lake build
✔ [2/3] Built ReviewGraph (1.3s)
Build completed successfully (3 jobs).
LAKE_EXIT=0

$ lake env lean ReviewGraph.lean
LEAN_CHECK_EXIT=0
```

Zero errors, zero warnings, zero `sorry` (`grep -c sorry ReviewGraph.lean` → 0).
48 theorems and 6 kernel-checked examples (toy fixtures from `example/ts-lsp-test`).

## Theorem gates by design decision

| Decision | Theorems |
|---|---|
| D1 identity | `keyOf_text_free`, `rank_zero_of_unique`, `countBefore_unrelated`, `rank_ge`, `head_ne_tail`, `assignKeys_distinct` |
| D2 imports / reachability | `importTarget_eq_moduleKey`, `placeholder_ne_moduleKey`, `moduleKey_mem_nodeKeys`, `resolved_import_kept`, `chain_lifts` |
| D3 resolution | `byKey_nil_of_absent`, `byKey_unique`, `byKey_of_mem`, `resolve_single_sound`, `resolve_notFound_sound`, `resolve_ambiguous_sound`, `exactId_eq_byKey`, `resolve_exact_id` |
| D4 cypher | `evalOld_subset_evalNew`, `evalNew_complete`, decided counter-example (`evalOld 1 … = []`, `evalNew 1 … = [2]`) |
| D5 observability | `noObservability_disables`, `old_ignores_flag`, `isPrefixSeg_append`, `relative_under_output` |
| D6 report | `bridgeRows_length_le`, `bridgeRows_degree` |
| D7 granularity | `cli_wins`, `perExt_wins`, `global_applies`, `wiring_threads_level`, `wiring_bug_refuted`, `wiring_fixed` |

## Methodology compliance (`lean-proof-methodology`)

- **Goal-guided invocation only.** Theorems are applied through `simp only [...]`,
  `rw`, `obtain`/`cases`, `subst`, and defeq-forcing `have`/`show` steps; no
  positional proof-term application of a theorem carrying an auto-implicit
  binder (`autoImplicit` is off file-wide).
- **No `by decide` over non-reducible shapes.** `decide` is used only on Nat
  lists (`evalOld`/`evalNew` counter-example), on the `Gran` enum
  (`wiring_bug_refuted`), and on the toy resolution fixtures whose only string
  operations are literal equalities (`String.decEq` kernel-reduces); every
  list primitive the kernel must reduce (`filt`, `tk`, `concatMap`,
  `Distinct`, `isPrefixSeg`) is a structural helper defined in the file.
- **Parametric, not stubbed.** The id renderer, the case normaliser and the
  bridge-table sort are section parameters (the sort constrained only by
  membership preservation), so the theorems hold for every implementation the
  Haskell mirrors may choose.
- **Linter hygiene.** `set_option linter.unusedSectionVars false` file-wide;
  the build is warning-free.
