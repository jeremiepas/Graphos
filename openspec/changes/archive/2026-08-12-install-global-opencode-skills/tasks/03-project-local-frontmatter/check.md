# Task 3 — Update project-local skill template with YAML frontmatter — CHECK

**Task slug**: `03-project-local-frontmatter`
**Attempt**: 1
**Status**: pending

## Summary

Validate that project-local opencode skill generation now includes frontmatter and that no other content changed unexpectedly.

## Detail

### Check Criteria (from plan.md)
- C1: `cabal build --flag dev` is warning-free.
- C2: Existing project-local opencode skill golden tests fail predictably with only a leading frontmatter diff.
- C3: The generated content still includes the version stamp immediately after the frontmatter.
- C4: No other generated content changes beyond the added frontmatter block.

### Execution
Pending implementation (`/opsx-apply`). Commands:
```bash
cabal build --flag dev
cabal test
# inspect golden diff if test fails
```

### Evidence
_TBD after implementation._

## Result

Pending `/opsx-apply`. Once executed, record PASS/FAIL per criterion and capture the diff.
