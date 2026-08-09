# Act: Tiered whitelist + recursion stop

- Boundary rule ("nothing inside a function body is visible at function level") documented in the module haddock.
- Convention: new grammar node types must be added to the correct tier, not to a flat list.
