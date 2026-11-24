---
name: mnemo-status
description: Inspect mnemo runs and step runs. Trigger when you need to list the runs of a space or see the state of one run (its step runs, and with --cards what was on each card).
---

# mnemo status

List the runs of a space, or show one run and its step runs.

```bash
mnemo status [RUN] [--space SPACE] [--cards]
```

- **No `RUN`**: lists the runs of the space (newest first).
- **With `RUN`**: shows that run and its step runs.
- **`--cards`**: under each step run, list the items that were on its card, each with the path that put it there.

For this repository use the wrapper: `mnemo-graphos status` — the `graphos` space has `0 runs` (it has no delivery board).
