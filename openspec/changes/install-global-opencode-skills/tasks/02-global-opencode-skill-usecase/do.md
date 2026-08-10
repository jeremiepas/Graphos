# Task 2 — Extend scaffold UseCase with global opencode skill planning — DO

**Task slug**: `02-global-opencode-skill-usecase`
**Attempt**: 1
**Status**: in-progress

## Summary

Extend the scaffold UseCase with a pure planner for global opencode skill installation that emits two file plans (full and query-only), both with YAML frontmatter and a parser-derived command reference.

## Detail

### What will be implemented
- Add `InstallSkillRequest` type:
  ```haskell
data InstallSkillRequest = InstallSkillRequest { installSkillTarget :: InstallSkillTarget }
  ```
- Add `installSkillPlan :: InstallSkillRequest -> CommandReference -> [FilePlan]`.
- For `OpencodeTarget`, return two plans:
  - Relative path `graphos/SKILL.md` with full-skill template.
  - Relative path `graphos-query/SKILL.md` with query-only template.
- Add a shared helper:
  ```haskell
skillFrontmatter :: Text -> Text -> Text
skillFrontmatter name description =
  "---\nname: " <> name <> "\ndescription: " <> description <> "\n---\n"
  ```
- Add `opencodeGlobalSkillTemplate` and `opencodeGlobalQuerySkillTemplate` that use the frontmatter helper plus the existing version-stamp helper and command-reference renderer.
- The query-only template includes prose: "This skill is read-only. Do NOT run `graphos build`, `graphos ... --update`, or `graphos ingest ...`."

### Key decisions
- Planner is pure and deterministic, returning relative paths under a configurable global skills root (Infrastructure supplies the absolute base).
- Both global templates reuse existing `renderCommandReference` so the skill reference stays synchronized with the CLI parser.

### Concrete changes
- `src/Graphos/UseCase/Scaffold.hs`: new types, planner, templates, and frontmatter helper.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
