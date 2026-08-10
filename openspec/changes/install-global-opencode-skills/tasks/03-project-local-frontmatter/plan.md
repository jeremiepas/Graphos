# Task 3 — Update project-local skill template with YAML frontmatter — PLAN

**Task slug**: `03-project-local-frontmatter`
**Attempt**: 1
**Status**: pending

## Summary

Update the existing project-local opencode skill template to include the same YAML `name`/`description` frontmatter block before the version stamp, without changing any other behavior.

## Detail

### Scope
- Refactor `opencodeSkillTemplate` to call the shared `skillFrontmatter` helper.
- Keep the existing body, command reference, and sub-agent contract prose unchanged.
- Do not update golden files in this task; use test failure diff to confirm the change is correct.

### Check Criteria (defined before code)
- C1: `cabal build --flag dev` is warning-free.
- C2: Existing project-local opencode skill golden tests fail predictably with only a leading frontmatter diff.
- C3: The generated content still includes the version stamp immediately after the frontmatter.
- C4: No other generated content changes beyond the added frontmatter block.

### Affected Modules
- `src/Graphos/UseCase/Scaffold.hs`

### Prerequisites
- Task 2 must provide the shared `skillFrontmatter` helper.

### Risks
- If the helper is shared incorrectly, claude/generic or sub-agent templates could accidentally change. Mitigation: only edit the opencode project-local skill template.
