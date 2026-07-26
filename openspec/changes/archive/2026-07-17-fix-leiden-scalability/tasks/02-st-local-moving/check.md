# Check: ST-based local moving pass

- `cabal test`: PASS — 145 examples, 0 failures; all three goldens unchanged (result equivalence proven).
- `cabal build`: clean with `-Wall -Werror`.
- Mutation confined to `runST`; module exports and purity unchanged.
