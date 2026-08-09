# Check: Stub extraction hygiene

## Test Results
- `cabal test`: PASS — 142 examples, 0 failures.
- `cabal build`: PASS with `-Wall -Werror`.

## Verification
- All new Haskell extraction specs passed.
- Integration run: zero truncated-junk labels; zero `kind=None` nodes from the stub path.
