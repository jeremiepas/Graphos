<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as a trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Update golden-file test fixtures for the new prose — PLAN

**Task slug**: `03-update-golden-file-test-fixtures`
**Attempt**: 1
**Status**: pending

## Summary

Update the golden-file test fixtures in `tests/fixtures/scaffold/` to match the new prose output from `Scaffold.hs` (task 1). The `agent-scaffolding` golden-file tests compare scaffold output against these fixtures. After task 1's template changes, the fixtures contain stale content that no longer matches the generated output.

## Detail

### Scope

Update golden fixture files:
- `tests/fixtures/scaffold/graphos-global-skill.md`
- `tests/fixtures/scaffold/graphos-query-global-skill.md`

These are plain-text fixtures read by `tests/Graphos/UseCase/ScaffoldSpec.hs` (line 134 and 140). No changes to test code or `Scaffold.hs`.

### Check Criteria (defined BEFORE code)

**Tests/gates to run:**
- `cabal test` — expected: exit code 0, all spec tests pass (specifically the `installSkill` describe block at `tests/Graphos/UseCase/ScaffoldSpec.hs:121`)
- `cabal build --flag dev` — expected: zero warnings, exit code 0

**Spec scenarios satisfied:**
- `agent-scaffolding/spec.md` — "Reference matches the parser" (scenario 43-45)
- `agent-scaffolding/spec.md` — "Skill names graphos CLI and graphos-out" (scenario 48-50)

**Exact PASS conditions:**
1. `cabal test` exits with code 0 (all specs pass, including the two golden-file comparisons)
2. The updated golden files contain the new "Your tools" / "Tool: graphos CLI" section from task 1
3. The updated golden files contain the guardrail line (warning against Python, `graphify`, and MCP tools)
4. The updated golden files still contain the parser-derived command reference (unchanged from the existing golden output — the command reference is rendered fresh from the parser at test time, so the golden file should retain it)

**FAIL boundaries:**
- Condition 1: `cabal test` fails with a diff between actual scaffold output and the golden file → FAIL (golden file does not match actual output; may indicate the prose templates did not produce expected content, or the golden was edited incorrectly)
- Condition 4: the golden file is missing the command reference section → FAIL (the parser reference is injected at generation time; if it's missing from the golden, the fixture was copied incorrectly)

### Affected modules

- `tests/fixtures/scaffold/graphos-global-skill.md` (golden fixture, text content updated)
- `tests/fixtures/scaffold/graphos-query-global-skill.md` (golden fixture, text content updated)

### Prerequisites

- Task 1 must complete: the prose templates must contain the new sections so the actual scaffold output includes them, and the golden files can be updated from the correct output.
- `cabal build` must pass (for `cabal test` to run). If blocked by `Extract/Core.hs:155`, the build gate cannot verify.

### Risks

- **Diff reveals unintended prose change**: The golden update might show that task 1's prose changes produced unexpected output (e.g., a missing blank line causes paragraphs to merge). Mitigation: inspect the diff before accepting; if the golden reveals a bug, record under Attempt history and start attempt 2.
- **Golden files include generated content that changes per run**: If the command reference or stamp version changes between runs, the golden comparison fails. Mitigation: verify the fixture is stable by running `cabal test` multiple times.
- **Build gate blocked**: If `Extract/Core.hs:155` is unresolved, `cabal test` cannot run. Record as a blocker.
