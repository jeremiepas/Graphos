# Plan: Stub extraction hygiene

## Goal
Stop emitting truncated and junk symbol nodes from the Haskell stub extractor; assign a `kind` to every declaration.

## Approach
- Tighten `isTopLevelDecl` to column-0 lines starting with a letter or `(`.
- Make `extractDeclName` return `Maybe String`; skip lines that yield no valid identifier.
- Add `declKind` classifier and use it in `haskellStubNodes`.
- Split `haskellStubEdges` into `imports` and `contains` relations.

## Check Criteria
- Junk lines (`| otherwise`, `}`, `where`, string fragments) produce no nodes.
- No emitted label equals a 20-char prefix of a non-identifier source line.
- Kinds match declaration forms: `data`/`newtype`/`type` → Type, `class` → Class, `instance` → Instance, other identifier → Function.
- `cabal test` and `cabal build` pass.

## Affected Files
- `src/Graphos/UseCase/Extract/Haskell.hs`
- `tests/Graphos/UseCase/Extract/HaskellSpec.hs`
- `graphos.cabal`
