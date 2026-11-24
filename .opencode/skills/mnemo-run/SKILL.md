---
name: mnemo-run
description: Run or resume a mnemo delivery workflow. Trigger on "run the workflow", "run the proposal", or resuming an interrupted run.
---

# mnemo run

```bash
mnemo run [WORKFLOW] [--param NAME=VALUE] [--max-steps N] [--resume RUN] [--space SPACE]
```

- Starts a run of `WORKFLOW`, or resumes a run.
- `--param NAME=VALUE` sets a workflow parameter (repeat for each).
- `--max-steps N` stops after N step runs, leaving the run resumable.
- `--resume RUN` continues a run from its stored state with the definition it started with.

**Exit codes**: `0` passed · `1` a step failed · `2` usage/environment error (model endpoint did not answer — the run is stopped, resumable) · `3` waiting for a person · `4` stopped by `--max-steps` with steps still to run.

**Run layer, not the delivery board**: `mnemo run` is verified working against `graphos` (its config carries the `profiles` and `delegates` the run layer needs — there is no `delivery` section, but the run layer does not need one). Only `mnemo delivery` (canvas, catalogue, workspaces) is never run against `graphos` — see AGENTS.md.
