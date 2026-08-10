# Task 4 — Add Infrastructure writer for global install with idempotency — CHECK

**Task slug**: `04-global-infrastructure-writer`
**Attempt**: 1
**Status**: pending

## Summary

Validate the Infrastructure writer for global install: directory creation, idempotent writes, skip messages, and no overwrites.

## Detail

### Check Criteria (from plan.md)
- C1: Writing to a temporary directory creates both `graphos/SKILL.md` and `graphos-query/SKILL.md`.
- C2: A second write to the same directory skips both files and exits 0.
- C3: If one file exists and the other does not, only the missing one is created; the existing one is skipped.
- C4: No existing file is overwritten (byte-identical after a skipped write).
- C5: Production code uses `getHomeDirectory` and does not touch test directories.
- C6: `cabal test` passes for the new writer tests.

### Execution
Pending implementation (`/opsx-apply`). Commands:
```bash
cabal test
```

### Evidence
_TBD after implementation._

## Result

Pending `/opsx-apply`. Once executed, record PASS/FAIL per criterion.
