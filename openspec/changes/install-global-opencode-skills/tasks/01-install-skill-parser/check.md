# Task 1 — Add `install-skill` CLI parser and wire to program entrypoint — CHECK

**Task slug**: `01-install-skill-parser`
**Attempt**: 1
**Status**: pending

## Summary

Validate the new `install-skill` subcommand and `--target` option by running the checks defined in `plan.md`.

## Detail

### Check Criteria (from plan.md)
- C1: `graphos --help` lists `install-skill` as a subcommand and `--target` as its option.
- C2: `graphos install-skill` (missing `--target`) exits with a usage error that mentions `--target`.
- C3: `graphos install-skill --target foo` exits with an error naming valid targets (`opencode`).
- C4: `graphos install-skill --target opencode` reaches the new handler.
- C5: `cabal build --flag dev` produces zero warnings.

### Execution
Pending implementation (`/opsx-apply`). Commands to run once code exists:
```bash
cabal build --flag dev
graphos --help
cabal test
```

### Evidence
_TBD after implementation._

## Result

Pending `/opsx-apply`. Once executed, record PASS/FAIL per criterion with exact command output and exit codes.
