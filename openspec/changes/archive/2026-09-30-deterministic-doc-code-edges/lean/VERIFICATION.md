# Verification command (tasks 1.4 / 7.3)

```bash
# Toolchain: Lean 4.29.1 core only, no Mathlib.
export PATH="/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin:$PATH"
cd openspec/changes/deterministic-doc-code-edges/lean
lake clean && lake build
# Exit 0, zero errors, zero warnings, zero sorry.
```

The toolchain comes from `nix run nixpkgs#lean4` (package `lean4`,
version 4.29.1). `nixpkgs#lean` must NOT be used: it provides Lean 3.

Run log (2026-09-18):

```
$ lake clean && lake build
✔ [2/3] Built DocLink (26s)
Build completed successfully (3 jobs).
LAKE_EXIT=0

$ lake env lean DocLink.lean
LEAN_CHECK_EXIT=0
```

Zero errors, zero warnings, zero `sorry`.

## Methodology compliance (`lean-proof-methodology`)

- **Goal-guided invocation only.** All theorem applications are via
  `simp only [...]`, `rcases`/`obtain`, `subst`, and direct `term := lemma h`
  constructions; no positional proof-term application of a theorem carrying
  an auto-implicit binder.
- **No `by decide` over non-reducible statement shapes.** The stdlib String
  API (`String.splitOn`, `String.isPrefixOf`, `String.take`, `String.drop`,
  `String.endsWith`) does not kernel-reduce in Lean 4.29.1 (its `Decidable`
  instances get stuck on `String.Pos` internals — probed and confirmed before
  writing the model). The artifact therefore builds every path/token
  primitive as a structural helper over `String.toList`/`String.ofList`
  (`splitChar`, `splitSlash`, `wordTokens`, `pathTokens`, `isPrefixL`,
  `endsWithStr`, `dropDotSlashL`, `dropTrailingPunct`), each of which
  kernel-reduces; the toy fixtures are checked by `rfl`, never by `decide`
  over `List.lookup`-bearing or String-API-bearing statements.
- **Float is not kernel-decidable.** Confidence is modeled as the
  kernel-decidable inductive `Confidence` (`documents'` ≥ semantic threshold
  / `inferred'` below), mirroring the Haskell `Confidence Double` ≥ 0.7
  semantic-filter rule (`survivesSemantic`).
- **`set_option linter.unusedSectionVars false`** is set file-wide.

## Guarantee → theorem map

| Guarantee | Theorem |
|---|---|
| Co-location edges stay inside the doc's directory subtree | `colocEdges_subtree` (via `colocEdges_shape`) |
| Symbol-mention edges target the unique definition of a whole-word identifier (length ≥ 4) | `symbolEdges_uniqueDef` |
| Path-reference edges resolve to an existing `source_file`, across subtrees | `pathRefEdges_resolves` |
| Every emitted edge is `documents` + survives the semantic filter | `docCodeEdges_semanticSafe` (via the three `*_semanticShape` lemmas) |