# Check: Force build/cluster spans + edge-collapse guard

## Test Results
- `cabal test`: PASS — 142 examples, 0 failures.
- `cabal build`: PASS with `-Wall -Werror`.
- Traced run: `span_build` = 16.2 ms, `span_cluster` = 232 ms (both ≥ 1 ms).

## Verification
- Span durations no longer report nanoseconds.
- No edge-collapse warning fired on this repo (ratio ≈ 1.2, well above 0.05).
