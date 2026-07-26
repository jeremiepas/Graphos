# Act: Canonical module IDs + relation semantics

## Standardized
- Canonical module IDs are the default for Haskell stub extraction.
- `Main` exception is documented in `canonicalModuleId` haddock and in design.md.

## Follow-up
- If other languages (TypeScript via tree-sitter, etc.) need cross-file edges, implement a similar per-language canonicalization in a dedicated change.
