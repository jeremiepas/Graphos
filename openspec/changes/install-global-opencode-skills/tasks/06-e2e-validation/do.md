# Task 6 — End-to-end validation and PDCA close-out — DO

**Task slug**: `06-e2e-validation`
**Attempt**: 1
**Status**: in-progress

## Summary

Execute the full validation suite, perform a safe manual smoke test, and finalize the PDCA cycle.

## Detail

### What will be implemented
- Run `cabal build --flag dev` and confirm zero warnings.
- Run `cabal test` and confirm all tests pass.
- Perform a smoke test using a temporary `HOME` directory (or the writer test path) to avoid touching the real `~/.agents/skills/`:
  - Run `graphos install-skill --target opencode`.
  - Inspect `~/.agents/skills/graphos/SKILL.md` and `~/.agents/skills/graphos-query/SKILL.md` for correct frontmatter and version stamp.
  - Run again and verify skip messages + exit 0.
- Verify the query-only skill body forbids build/update/ingest.

### Key decisions
- Manual smoke test uses an isolated `HOME` to avoid modifying the user's real environment.
- Any issues found become a new PDCA attempt, not silent fixes.

### Concrete changes
- No code changes; this is validation only.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
