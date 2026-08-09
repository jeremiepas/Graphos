# Check: Lazy trace-directory creation

## Test Results
- `cabal test`: PASS — 142 examples, 0 failures (including the three new SDK specs).
- `cabal build`: PASS with dev `-Wall -Werror` flags.

## Verification
- SDK spec cases passed for disabled, enabled-but-empty, and enabled-with-events.
- No stray `traces/` directory observed on the default integration run.
