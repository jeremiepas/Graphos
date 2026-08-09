# Check: Tiered whitelist + recursion stop

- `cabal test`: PASS — 170 examples, 0 failures.
- fine: statement nodes + JSON pairs present (backward compatible).
- function: module/class/method/field/import/module-const present; nothing from inside function bodies; no contains-edges into bodies.
- file: exactly 1 node, 0 edges (both fixtures).
- `cabal build`: clean.
