# Proposal: openspec-view

## Why

The Graphos project maintains a growing OpenSpec workspace with 20+ changes, 80+ specs, and multiple artifact types (proposals, specs, designs, tasks). There is no unified view of the OpenSpec state — developers must manually navigate directories, run separate CLI commands, and cross-reference multiple sources to understand what has been proposed, what is in progress, what is complete, and what artifacts exist.

This creates friction when:
- Onboarding new agents to the OpenSpec workflow
- Auditing which changes are active vs archived
- Understanding the relationship between specs and delta specs
- Tracking implementation progress against change artifacts

## What Changes

Add an `openspec-view` capability that provides a structured, queryable view of the entire OpenSpec workspace state, including:

1. **Change listing** — All changes with status, task progress, and last modified timestamp
2. **Artifact browser** — Navigate proposal, specs, design, and tasks for any change
3. **Spec delta viewer** — Compare delta specs against main specs to see what changed
4. **State dashboard** — Aggregate view of all changes by status (in-progress, complete, archived)

## Capabilities

| # | Capability | Description |
|---|-----------|-------------|
| 1 | `openspec-change-list` | List all changes with status, task counts, and timestamps |
| 2 | `openspec-artifact-view` | Display a specific change's artifacts (proposal, specs, design, tasks) |
| 3 | `openspec-spec-diff` | Show delta spec changes compared to main specs |
| 4 | `openspec-state-dashboard` | Aggregate dashboard of all OpenSpec states and progress |

## Impact

- **No code changes** to Graphos core — this is a workspace metadata tool
- **No new dependencies** — uses existing OpenSpec CLI and file structure
- **Adds new OpenSpec conventions** for artifact linking and state tracking
- **Improves agent onboarding** by providing clear visibility into workflow state
