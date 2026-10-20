# VERIFICATION.md — Lean 4 machine check for `EmbedPerf.lean`

## Toolchain provenance

- Invoked via `nix run nixpkgs#lean4` (nixpkgs-unstable).
- **Exact store path:** `/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1`
- **Version:** Lean (version 4.29.1, commit v4.29.1, Release)
- ⚠️ **Warning:** `nixpkgs#lean` is **Lean 3** — always use `nixpkgs#lean4`.

## Exact commands

```sh
cd openspec/changes/lfm-embedding-optimization/lean
nix shell nixpkgs#lean4 -c lake clean
nix shell nixpkgs#lean4 -c lake build
```

## Green run log (2026-09-22)

```
$ nix shell nixpkgs#lean4 -c lake clean
info: EmbedPerf: no previous manifest, creating one from scratch
info: toolchain not updated; no toolchain information found
Build completed successfully (0 jobs).

$ nix shell nixpkgs#lean4 -c lake build
Build completed successfully (0 jobs).
```

`lake build` output contains **zero errors and zero warnings** (verified:
`grep -cE "error|warning"` over the build output returns 0).

## Reproduction (fresh shell, 2026-09-22)

A second `lake clean && lake build` from a fresh shell reproduced exit 0:

```
$ nix shell nixpkgs#lean4 -c lake clean && nix shell nixpkgs#lean4 -c lake build
Build completed successfully (0 jobs).
```

## What is proved

`EmbedPerf.lean` extends the archived `EmbedPipeline.lean` master theorems
(`pipeline_eq_baseline`, `runCached_fst`, `runCached_sound`, `chunks_flatten`,
`batchedTable_eq_table`) with the four lemmas required by the change:

1. `pipeline_prepared_eq_baseline` — preparation transform (D1/D3): the
   optimized pipeline instantiated at `f ∘ prepare` equals the prepared
   baseline.
2. `runMissesOnly_sound` / `hits_untouched` / `runMissesOnly_fst` —
   misses-only write-back (D5): the cache stays sound, prior entries appear
   verbatim (hits never rewritten), and the assignment is baseline-equal.
3. `prepared_convergence` / `prefix_change_invalidates` — prefix composition
   (D4): identical truncations converge on one submit text/key; a prefix
   change yields a different key.
4. `streamed_eq_write_at_end` / `streamed_entries_flatten` — streaming
   staged per-batch append then rename produces sidecar content equal to
   write-at-end (flattening associativity over `chunks_flatten`, D9).

Methodology compliance (lean-proof-methodology): goal-guided invocation of
`runCached_fst` / `runCached_sound` (no positional cache arguments),
`rcases beq : (a == k)` for lookup goals, no `by decide` on lookup-bearing
statements, and `set_option linter.unusedSectionVars false` at file top.