# Task 3 — Update project-local skill template with YAML frontmatter — DO

**Task slug**: `03-project-local-frontmatter`
**Attempt**: 1
**Status**: in-progress

## Summary

Refactor the existing project-local opencode skill template to prepend the shared YAML frontmatter block before the version stamp, leaving all other content unchanged.

## Detail

### What will be implemented
- Locate `opencodeSkillTemplate` in `UseCase/Scaffold`.
- Replace its opening version-stamp line with a call to `skillFrontmatter "graphos" "..."` followed by the existing version-stamp helper.
- Ensure the `description` is concise and matches the project-local skill's purpose.

### Key decisions
- Use the same `skillFrontmatter` helper created in Task 2 to keep formatting consistent across all skill templates.
- Only modify the project-local opencode skill template; claude/generic templates and sub-agent templates are out of scope.

### Concrete changes
- `src/Graphos/UseCase/Scaffold.hs`: adjust `opencodeSkillTemplate` composition.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
