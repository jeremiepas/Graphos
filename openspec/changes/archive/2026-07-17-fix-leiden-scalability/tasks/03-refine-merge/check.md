# Check: Batched refinement + incremental merge index

- `cabal test`: PASS — 147 examples, 0 failures.
- Goldens unchanged (refinement batching is result-equivalent).
- Node-loss regression case passes: triangle{a,b,c}+pair{d,e} now yields all 5 nodes ({a,b,c,d,e} in one community); previously `b` was silently dropped.
- `cabal build`: clean with `-Wall -Werror`.
