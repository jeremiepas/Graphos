<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Rename global skill graphify to graphos and update CLAUDE.md — PLAN

**Task slug**: `04-rename-global-skill-graphify-to-graphos`
**Attempt**: 1
**Status**: pending

## Summary

Rename the user-installed global skill from `~/.claude/skills/graphify/` to `~/.claude/skills/graphos/` and update all references within the renamed skill (`SKILL.md`, `references/*.md`) and the `~/.claude/CLAUDE.md` integration note. This eliminates the name collision where a `graphify` skill shadows the repo-local `graphos` skill.

## Detail

### Scope

User-environment files (outside the repo). Filesystem operations and text edits:
- `mv ~/.claude/skills/graphify ~/.claude/skills/graphos`
- Edit `~/.claude/skills/graphos/SKILL.md`: `name: graphify` → `name: graphos`, `# /graphify` → `# /graphos`, `graphify-out/` → `graphos-out/` throughout
- Edit `~/.claude/skills/graphos/references/*.md`: `graphify-out/` → `graphos-out/` throughout
- Edit `~/.claude/CLAUDE.md`: heading `# graphify` → `# graphos`, path `~/.claude/skills/graphify/SKILL.md` → `~/.claude/skills/graphos/SKILL.md`, trigger `/graphify` → `/graphos`

### Check Criteria (defined BEFORE code)

**Tests/gates to run:**
- `ls ~/.claude/skills/` — expected: `graphos/` present, `graphify/` absent
- `head -5 ~/.claude/skills/graphos/SKILL.md` — expected: shows `name: graphos` and `/graphos`
- `grep -r "graphify-out" ~/.claude/skills/graphos/` — expected: no matches
- `grep -r "/graphify" ~/.claude/skills/graphos/ ~/.claude/CLAUDE.md` — expected: no matches
- `grep -r "graphify" ~/.claude/skills/graphos/SKILL.md ~/.claude/CLAUDE.md` — expected: no matches (the only acceptable mention would be an explicit "do not use" line, but the renamed skill should have none)
- `cat ~/.claude/CLAUDE.md` — expected: references `~/.claude/skills/graphos/SKILL.md` and `/graphos`

**Spec scenarios satisfied:**
- `agent-scaffolding/spec.md` — "Global skill directory renamed" (scenario 67-70)
- `agent-scaffolding/spec.md` — "Global skill name and trigger are graphos" (scenario 72-75)
- `agent-scaffolding/spec.md` — "Global skill references graphos-out" (scenario 77-80)
- `agent-scaffolding/spec.md` — "CLAUDE.md references graphos" (scenario 82-85)

**Exact PASS conditions:**
1. `~/.claude/skills/graphos/SKILL.md` exists AND `~/.claude/skills/graphify/` does not exist
2. `~/.claude/skills/graphos/SKILL.md` shows `name: graphos` (not `graphify`) and contains `/graphos` trigger
3. `grep -r "graphify-out" ~/.claude/skills/graphos/` returns nothing
4. `grep -r "/graphify" ~/.claude/skills/graphos/ ~/.claude/CLAUDE.md` returns nothing
5. `grep -r "graphify" ~/.claude/skills/graphos/SKILL.md ~/.claude/CLAUDE.md` returns nothing (no `graphify` references remain at all)
6. `~/.claude/CLAUDE.md` references `~/.claude/skills/graphos/SKILL.md` and the `/graphos` trigger

**FAIL boundaries:**
- Condition 1: `graphify/` directory still exists or `graphos/` does not exist → FAIL (mv failed)
- Condition 3 or 4: `graphify-out` or `/graphify` strings survive in the renamed skill or CLAUDE.md → FAIL (sed/replacement missed some occurrences)
- Condition 5: the `graphifyy` Python package name was accidentally sed'd to `graphosy` → FAIL (must NOT rename the double-y package name; restore from backup)
- Condition 6: CLAUDE.md still references the old path or trigger → FAIL (CLAUDE.md not updated)

### Affected modules

None (user-environment files only, outside the repository).

### Prerequisites

- The global skill must exist at `~/.claude/skills/graphify/`. If the user has not installed it, this task is N/A (record as PASS by default).
- The user must have write access to `~/.claude/skills/` and `~/.claude/CLAUDE.md`.

### Risks

- **Accidental sed of `graphifyy` Python package**: A blanket `sed 's/graphify/graphos/g'` would rename the double-y Python package `graphifyy` to `graphosy`. Mitigation: only sed targeted patterns (`graphify-out`, `/graphify`, `name: graphify`, `# /graphify`). NEVER a blanket `graphify`→`graphos` replacement.
- **CLAUDE.md references other `graphify` tools**: If the file mentions `graphify` in a different context (e.g., as a general Python package name, not the skill), those references should NOT be changed. Mitigation: hand-edit CLAUDE.md rather than using sed; review each occurrence.
- **User has already renamed the skill**: The skill might already be at `~/.claude/skills/graphos/`. Mitigation: check existence before mv; if already correct, verify content and record PASS.
