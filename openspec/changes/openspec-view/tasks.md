# Tasks: openspec-view

## Phase 1: Foundation (dependency: none)

- [ ] **T1: Create openspec-view module structure** — Add `openspec-view` module to the Graphos project with submodules for each capability. Define module interfaces.
  - Check: `cabal build` succeeds with new module
- [ ] **T2: Implement change list data model** — Define types for `ChangeInfo` (name, status, completedTasks, totalTasks, lastModified) and file parser for `openspec/changes/` directory.
  - Check: Unit tests parse existing changes correctly

## Phase 2: Core capabilities (dependency: T1, T2)

- [ ] **T3: Implement change listing** — Implement `openspec-change-list` capability: scan `openspec/changes/`, compute status from task completion, sort by lastModified.
  - Check: Output matches `openspec list --json` for all existing changes
- [ ] **T4: Implement artifact viewer** — Implement `openspec-artifact-view` capability: read and display proposal.md, specs/, design.md, tasks.md for a given change.
  - Check: Displays all artifacts for `honor-graphosignore` change correctly
- [ ] **T5: Implement spec diff** — Implement `openspec-spec-diff` capability: compare delta specs against main specs, categorize as added/modified/removed.
  - Check: Diff output correctly identifies changes in a change with modified specs
- [ ] **T6: Implement state dashboard** — Implement `openspec-state-dashboard` capability: aggregate statistics, per-change progress, risk highlighting.
  - Check: Dashboard shows correct counts for all existing changes

## Phase 3: Integration (dependency: T3, T4, T5, T6)

- [ ] **T7: Add CLI subcommands** — Wire up `openspec view changes`, `openspec view artifacts`, `openspec view diff`, `openspec view dashboard` subcommands.
  - Check: All four subcommands execute without error
- [ ] **T8: Add depth level support** — Implement summary/full/sections depth levels for artifact viewer.
  - Check: Each depth level produces correctly scoped output
- [ ] **T9: Add status filter support** — Implement status filter for change list.
  - Check: Filtered output matches expected subset
- [ ] **T10: Write integration tests** — End-to-end tests for all four capabilities against real OpenSpec data.
  - Check: All integration tests pass with `cabal test`

## Phase 4: Documentation (dependency: T7)

- [ ] **T11: Document CLI usage** — Add help text for all four subcommands with examples.
  - Check: `openspec view --help` shows all subcommands
- [ ] **T12: Document OpenSpec conventions** — Document the artifact linking and state tracking conventions introduced by this change.
  - Check: Conventions are clear and actionable for new changes
- [ ] **T13: Add changelog entry** — Document the new feature in CHANGELOG.md.
  - Check: CHANGELOG.md includes openspec-view entry under Unreleased
