<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Update Scaffold.hs prose templates to declare graphos CLI as the toolset — PLAN

**Task slug**: `01-update-scaffold-hs-prose-templates`
**Attempt**: 1
**Status**: pending

## Summary

Update the five prose templates (`opencodeNavigatorProse`, `claudeNavigatorProse`, `opencodeSkillProse`, `claudeSkillProse`, `genericProse`) in `src/Graphos/UseCase/Scaffold.hs` to explicitly frame the `graphos` CLI as the agent's toolset, name `graphos-out/` as the output directory, and add guardrail lines warning against Python, the unrelated `graphify` tool, and unregistered `graphos_*` MCP tools.

## Detail

### Scope

Edit `src/Graphos/UseCase/Scaffold.hs` only. Add new prose lines to each of the five `*Prose` bindings (the `unlines` string-list definitions). No changes to types, functions, parser logic, or any other module.

### Check Criteria (defined BEFORE code)

**Tests/gates to run:**
- `cabal build --flag dev` — expected: zero warnings, exit code 0
- `cabal test` — expected: green (this task does NOT update golden files; that is task 3. Expected result: golden tests may fail here, recorded as a known blocker)

**Spec scenarios satisfied:**
- `agent-scaffolding/spec.md` — "Sub-agent prose declares graphos CLI as toolset" (scenario 24)
- `agent-scaffolding/spec.md` — "Sub-agent prose forbids Python, graphify, and MCP tools" (scenario 29)
- `agent-scaffolding/spec.md` — "Skill names graphos CLI and graphos-out" (scenario 48)

**Exact PASS conditions:**
1. `opencodeNavigatorProse` contains a "Your tools" section listing all six `graphos` subcommands (`query`, `path`, `explain`, `symbols`, `neighbors`, `ingest`) plus build/refresh
2. `opencodeNavigatorProse` contains the string `graphos-out/`
3. `opencodeNavigatorProse` contains the guardrail line mentioning `Python`, `graphify`, and `MCP` tools
4. `claudeNavigatorProse` contains the same three elements (tools section, `graphos-out/`, guardrail)
5. `opencodeSkillProse` contains a "Tool: graphos CLI" section, `graphos-out/graph.json`, and the guardrail
6. `claudeSkillProse` contains the same three elements
7. `genericProse` contains the same three elements
8. `cabal build --flag dev` — zero warnings, exit code 0

**FAIL boundaries:**
- Any of conditions 1-7: a prose template missing the tools section, missing `graphos-out/`, or missing the guardrail line → FAIL
- Condition 8: `cabal build` produces warnings or errors attributable to this task's changes → FAIL
- If the `unlines` list formatting is broken (missing blank line causing two prose lines to merge) → FAIL (this would cascade into golden test failures in task 3)

### Affected modules

- `src/Graphos/UseCase/Scaffold.hs` — five `*Prose` bindings

### Prerequisites

- `Extract/Core.hs:155` parse error must be resolved for `cabal build` to pass (pre-existing, outside scope; if blocked, record the blocker and proceed)

### Risks

- **`unlines` formatting**: Adding blank lines between prose sections is easy to get wrong; a missing blank line causes two paragraphs to merge into one. Mitigation: follow the existing blank-line pattern used in the templates.
- **Build gate blocked by `Extract/Core.hs:155`**: This pre-existing error prevents `cabal build`. Mitigation: record the blocker in the plan; do not fix it here.
- **Golden test cascade**: The prose changes will cause golden-file test failures (task 3). This is expected; task 3 handles golden updates.
