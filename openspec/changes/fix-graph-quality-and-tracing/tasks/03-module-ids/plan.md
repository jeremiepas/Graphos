# Plan: Canonical module IDs + relation semantics

## Goal
Enable cross-file `imports` edges by canonicalizing Haskell module/import node IDs and split edge relations correctly.

## Approach
- Introduce `canonicalModuleId` returning `mod_<ModuleName>` for all modules except `Main`, which keeps a directory-scoped ID.
- Use the canonical ID for both the module node and import-target nodes so they merge across files.
- Keep declaration IDs file-scoped (decl names are not globally unique).
- Update `haskellStubEdges` to emit `imports` only to import nodes and `contains` to declarations.

## Check Criteria
- Two files with an import relationship share one module node and have a cross-file `imports` edge.
- Two `Main` modules in different directories remain distinct.
- One-import-one-decl file yields exactly one `imports` and one `contains` edge.
- `cabal test` and `cabal build` pass.

## Affected Files
- `src/Graphos/UseCase/Extract/Haskell.hs`
- `tests/Graphos/UseCase/Extract/HaskellSpec.hs`
