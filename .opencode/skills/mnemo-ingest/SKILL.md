---
name: mnemo-ingest
description: Ingest skills, agents, commands, instruction files and tool manifests into the mnemo memory graph. Trigger when a skill, a command, AGENTS.md or another instruction file changes and must be reflected in memory.
---

# mnemo ingest

Read skills, agents, commands, instruction files and tool manifests into the memory graph.

```bash
mnemo ingest ROOT... [--space SPACE]
```

- `ROOT` is one or more directories or files to walk.
- For this repo use the wrapper `mnemo-ingest-graphos`, which reads `.opencode/skills/*/SKILL.md`, `.opencode/commands/*.md` and `AGENTS.md` into the `graphos` space. Its root is the symlink mirror `~/.local/share/mnemo/ingest/graphos` (ingesting the checkout directly would see the duplicate skills of `.worktrees/` and refuse).
- Run it after editing a skill, a command, or `AGENTS.md`; a second run is a no-op.
