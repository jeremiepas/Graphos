# Verification command (task 1.2)

```bash
# Toolchain: Lean 4.29.1 (core only, no Mathlib)
export PATH="/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin:$PATH"
cd openspec/changes/streaming-embeddings/lean
lake clean && lake build
# Exit 0, zero errors, zero warnings.
```

Run log (2026-09-17):

```
✔ [2/3] Built EmbedPipeline (286ms)
Build completed successfully (3 jobs).
LAKE_EXIT=0
```

Re-verification after implementation (2026-09-18, task 6.2):

```
$ lake build
Build completed successfully (3 jobs).
$ lake env lean EmbedPipeline.lean
LEAN_CHECK_EXIT=0
```

Zero errors, zero warnings — Lean artifact still compiles clean against the
final implementation.

The toolchain comes from `nix run nixpkgs#lean4` (package `lean4`,
version 4.29.1). `nixpkgs#lean` must NOT be used: it provides Lean 3.