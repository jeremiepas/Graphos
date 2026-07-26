# Do: Canonical module IDs + relation semantics

## Changes Made
- `src/Graphos/UseCase/Extract/Haskell.hs`
  - Added `canonicalModuleId` with the `mod_<Name>` scheme and the `Main` exception.
  - Used it for module nodes and import-target nodes; declaration nodes still use `dirHash_declName`.
  - Updated `haskellStubEdges` to classify nodes by `nodeKind == Just "Module"` and emit `imports`/`contains` accordingly.
- `tests/Graphos/UseCase/Extract/HaskellSpec.hs`
  - Added Hspec cases verifying shared module IDs across files and distinct `Main` module IDs.
