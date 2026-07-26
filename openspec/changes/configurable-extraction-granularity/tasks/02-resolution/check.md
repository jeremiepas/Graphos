# Check: Resolution order + CLI flag

- `cabal test`: PASS — 163 examples, 0 failures (4 new resolution cases).
- `cabal run graphos -- --help` shows `--granularity LEVEL` with the documented default.
- `cabal build`: clean.
