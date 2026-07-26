# Check: Granularity type + config plumbing

- `cabal test`: PASS — 159 examples, 0 failures (11 new ConfigSpec cases).
- `cabal build`: clean with `-Wall -Werror` (lib + exe + tests).
- Round-trip, unknown-rejection ("fine, function, or file" in error), default and merge-precedence assertions all green.
