# Act: Stub extraction hygiene

## Standardized
- Column-0 / skip-instead-of-truncate rules are documented in the module haddock.
- Kind classification table (`data`/`newtype`/`type`→Type, `class`→Class, `instance`→Instance, else Function) is now the convention for stub extraction.

## Follow-up
- Apply the same hygiene rules to the tree-sitter/TypeScript fallback if it starts emitting junk labels.
