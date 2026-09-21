## 1. Lean formal model (reference for every Domain mirror)

- [x] 1.1 Model the three passes in `lean/DocLink.lean` (Lean 4 core, no Mathlib): `CodeGraph` with typed nodes (`doc` / `code` kinds, source paths, symbol definitions), a `text` field on doc nodes, and edge constructors `colocEdges` / `symbolEdges` / `pathRefEdges` / `documents`-relation tagging
- [x] 1.2 Prove the guarantee theorems: (a) `colocEdges_subtree` — every co-location edge stays within the doc's directory subtree; (b) `symbolEdges_uniqueDef` — every symbol-mention edge targets the unique definition of a whole-word identifier of length ≥ 4; (c) `pathRefEdges_resolves` — every path-reference edge resolves to an existing `source_file` and carries the `documents` relation; (d) `docCodeEdges_semanticSafe` — every emitted edge carries `documents` confidence (kernel-decidable mirror of ≥ 0.7, survives the semantic filter)
- [x] 1.3 Follow `lean-proof-methodology` rules: goal-guided invocation (`simp only`/`rcases`/`subst`, no positional proof terms), no `by decide` over non-kernel-reducible statements (String stdlib API avoided — custom `splitChar`/`wordTokens`/`pathTokens`/`endsWithStr` helpers that reduce by `rfl`), `set_option linter.unusedSectionVars false` file-wide
- [x] 1.4 Recorded the pinned toolchain and exact command in `lean/VERIFICATION.md` (`nix run nixpkgs#lean4` → Lean 4.29.1 core at `/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin`; warns that `nixpkgs#lean` is Lean 3); `lake clean && lake build` exits 0 with zero errors, zero warnings, zero `sorry` (run log 2026-09-18)
- [x] 1.5 Kernel-checked toy fixtures in the artifact: sibling README↔code linked (100→200, 100→201), unrelated-dir doc not linked (102 isolated, 202 not linked by co-location), unique symbol mention linked (validateToken→200, parseConfig→201), common word skipped (config/handler not defined), cross-subtree path citation linked (101→202), dangling path and bare filename skipped (./src/lib.rs dangling for 100; index.ts bare) — all by `rfl`

## 2. Domain

- [x] 2.1 Add `documents` edge relation and confidence tagging to the edge model (already present in `src/Graphos/Domain/Types/Edge.hs` — confirm serialization and no drift)
- [x] 2.2 Ensure `documents` is classified into the semantic (non-ambiguous) edge set
- [x] 2.3 Tests: edge relation serialization and semantic-set membership
- [x] 2.4 Checker parity: run the Domain linking passes on the Lean toy fixtures and assert verdicts identical to the Lean model (same discipline as `intent-graph-verification`)

## 3. Co-location pass

- [x] 3.1 Implement pure function linking a doc node to code in same/descendant directory
- [x] 3.2 Guard against cross-subtree links (top-level README stays in its subtree)
- [x] 3.3 Tests: sibling link created, unrelated-dir link not created

## 4. Symbol-mention pass

- [x] 4.1 Reuse the symbol/definition index to resolve identifiers mentioned in doc text
- [x] 4.2 Match whole-word identifiers length ≥ 4 with exactly one definition; skip ambiguous
- [x] 4.3 Tests: defined-symbol mention links; common-word and ambiguous cases skipped

## 5. Path-reference pass

- [x] 5.1 Implement pure function resolving explicit repo-relative file paths in doc text to code nodes by `source_file`
- [x] 5.2 Match path-like tokens that contain a `/` separator and a known source extension; normalize a leading `./`; require the path to resolve to an existing node (skip dangling paths)
- [x] 5.3 Link across directory subtrees (unlike co-location); tag as `documents` high confidence
- [x] 5.4 Tests: cited path links doc→file nodes across subtrees; non-existent path and bare filename (no separator) skipped

## 6. Pipeline integration

- [x] 6.1 Wire the three passes (co-location, symbol-mention, path-reference) into the Infer stage after edge inference
- [x] 6.2 Keep similarity `inferred` edges but ensure separation from `documents`
- [x] 6.3 Make thresholds (min identifier length, path extensions) configurable

## 7. Verification

- [x] 7.1 `cabal build --flag dev` with `-Werror`
- [x] 7.2 `cabal test` green including linking suites
- [x] 7.3 `cd lean && lake clean && lake build` green, zero `sorry` (task 1.4 command)
- [x] 7.4 Query a documented component with `edges=semantic`; confirm doc AND code nodes returned
