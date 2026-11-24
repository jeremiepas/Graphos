---
name: mnemo-workflow
description: Create or update a mnemo delivery workflow and its step/task definitions. Trigger on "new workflow", "new task", or adding a step, via workflow import.
---

# mnemo workflow (new workflow / new task)

A workflow is a directed acyclic graph of steps. Create it — and its steps/tasks — from a definition file.

```bash
mnemo workflow import FILE [--space SPACE]
```

- Checks the definition **whole**, then writes the workflow and its steps; importing an unchanged file changes nothing.
- **"new task"** = add or edit a step in the definition file and re-import.
- A step carries its `id`, `name`, `model`, `needs`, `do` (the `do` lines) and `done_when`; the catalogue fills what the definition does not carry.
- **Run layer, not the delivery board**: `mnemo workflow` belongs to the workflow/run layer, verified working against `graphos` (the import needs no `delivery` config section, only the memory graph). Only `mnemo delivery` (canvas, catalogue, workspaces) is never run against `graphos` — see AGENTS.md.
