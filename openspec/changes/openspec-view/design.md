## Context

The Graphos OpenSpec workspace has grown to 20+ changes, 80+ specs, and multiple artifact types. Currently, understanding the workspace state requires running multiple `openspec` CLI commands, manually browsing directories, and cross-referencing information. There is no single view that shows the full state of the OpenSpec workspace.

## Goals / Non-Goals

**Goals:**
- Provide a unified view of all OpenSpec changes with status and progress
- Enable artifact browsing for any change
- Show spec diffs between delta specs and main specs
- Display an aggregate dashboard of workspace state

**Non-Goals:**
- Modifying existing OpenSpec artifacts or workflow
- Adding new OpenSpec schema types
- Replacing the `openspec` CLI — this extends it with a view layer

## Decisions

- **File-based metadata aggregation** — Read directly from `openspec/changes/` and `openspec/specs/` directories rather than maintaining a separate state database.
  - *Alternative considered:* SQLite state file — rejected, adds dependency and sync complexity.
- **CLI subcommands** — Expose as `openspec view changes`, `openspec view artifacts`, `openspec view diff`, `openspec view dashboard`.
  - *Alternative considered:* HTTP server — rejected, overkill for a metadata viewer.
- **Markdown output** — Use markdown-formatted output for human readability and easy integration with existing tooling.
  - *Alternative considered:* JSON-only — rejected, harder for humans to read directly.

## Risks / Trade-offs

- [Performance with large workspaces] → Stream output for large changes; paginate if needed.
- [Stale data risk] → Always read from disk at request time; no caching layer.
- [Scope creep] → Keep the four capabilities scoped; defer any additional views.
