# Task 5 — Update golden files and add global-install golden tests — DO

**Task slug**: `05-golden-files`
**Attempt**: 1
**Status**: in-progress

## Summary

Update project-local opencode skill golden fixtures to include frontmatter, add global skill golden fixtures, and add golden tests for the global-install planner and writer.

## Detail

### What will be implemented
- Regenerate/update `test/fixtures/scaffold/opencode-skill.md` (or equivalent) to include the new frontmatter.
- Add:
  - `test/fixtures/scaffold/opencode-global-skill.md`
  - `test/fixtures/scaffold/opencode-global-query-skill.md`
- Add unit tests for `installSkillPlan` comparing output to the new golden files.
- Add writer test using `runInstallSkillWithRoot` with a temporary directory:
  - First run creates both files and matches golden content.
  - Second run skips both files.
  - Partial pre-existing file test: create only one file, run again, assert the other is created and the existing one is unchanged.

### Key decisions
- Golden files are generated from the UseCase planner output to avoid hand-editing.
- Review diffs carefully; unexpected changes indicate source bugs, not fixture drift.

### Concrete changes
- Test module for scaffold (e.g., `test/Graphos/ScaffoldSpec.hs`).
- Fixture files under `test/fixtures/scaffold/`.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
