---
name: mnemo-cancel
description: Cancel a non-final mnemo run. Trigger to abandon an in-flight run.
---

# mnemo cancel

```bash
mnemo cancel RUN --by NAME [--reason TEXT] [--space SPACE]
```

- Fails the step runs the run left running with reason `cancelled`, keeps every other step run, approval and answer, and leaves the run cancelled and unresumable.
- Refuses a run another process holds, or one already passed/failed/cancelled — nothing is written.

**Exit codes**: `0` cancelled · `1` refused (nothing written) · `2` usage/environment error.

**Run layer, not the delivery board**: `mnemo cancel` belongs to the workflow/run layer (it abandons a non-final run, no delivery config involved), so it is usable against `graphos`. Only `mnemo delivery` is never run against `graphos` — see AGENTS.md.
