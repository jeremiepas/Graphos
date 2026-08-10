# Task 5 — Update golden files and add global-install golden tests — PLAN

**Task slug**: `05-golden-files`
**Attempt**: 1
**Status**: pending

## Summary

Regenerate the project-local opencode skill golden fixture to include frontmatter, add golden fixtures for the two global skills, and add tests covering `install-skill --target opencode` output and idempotency.

## Detail

### Scope
- Update existing golden file(s) for `.opencode/skills/graphos/SKILL.md` to include the new frontmatter.
- Add new golden files for `~/.agents/skills/graphos/SKILL.md` and `~/.agents/skills/graphos-query/SKILL.md`.
- Add golden tests invoking the UseCase planner for global install.
- Add golden/idempotency tests for the Infrastructure writer using a temporary directory.

### Check Criteria (defined before code)
- C1: All scaffold-related golden tests pass after updates.
- C2: New global golden files match planner output exactly.
- C3: `cabal test` is green.
- C4: No golden file is hand-edited to hide source bugs; any unexpected diff is fixed in code.

### Affected Modules
- Test module(s) for scaffold (e.g., `test/Graphos/ScaffoldSpec.hs`)
- Golden fixture directories under `test/fixtures/`

### Prerequisites
- Tasks 2, 3, and 4 completed.

### Risks
- Large golden diffs can mask real bugs. Mitigation: review diffs carefully before accepting.
