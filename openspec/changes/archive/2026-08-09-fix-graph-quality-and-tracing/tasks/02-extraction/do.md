# Do: Stub extraction hygiene

## Changes Made
- `src/Graphos/UseCase/Extract/Haskell.hs`
  - Rewrote `isTopLevelDecl` to require column-0 start with letter or `(`.
  - Changed `extractDeclName` signature to `String -> Maybe String` and removed the `take 20` fallback.
  - Added `declKind` classifier.
  - Updated `parseHaskellDecls` to return `[(String, Text)]` with kind.
  - Updated `haskellStubNodes` to skip `Nothing` decls and set `nodeKind`.
  - Exported `haskellStubNodes` and `haskellStubEdges` for testing.
  - Updated `haskellStubEdges` to emit `Imports` to import/module nodes and `Contains` to declarations.
- `tests/Graphos/UseCase/Extract/HaskellSpec.hs`
  - Added Hspec cases for junk-line rejection, no truncated labels, kind assignment, and relation semantics.
- `graphos.cabal`
  - Listed `Graphos.UseCase.Extract.HaskellSpec` under `other-modules`.
