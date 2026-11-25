---
name: mnemo-card
description: Preview the card a mnemo step run would receive. Trigger to inspect what a step would see (its tier/size/score/path inputs) without writing anything or calling a model.
---

# mnemo card

```bash
mnemo card WORKFLOW STEP [--param NAME=VALUE] [--focus PATH] [--space SPACE]
```

- Shows the card a step run of `STEP` would be given now: each item with its tier, size, score and path, the total, and the candidates left out with the reason.
- **Nothing is written and no model is called.**
- `--param graph_project=NAME` names the Graphos project of the focus paths; `--focus PATH` sets a repository path the step run would focus on (repeat for each).

**Exit codes**: `0` a card · `1` the declared content and the inputs do not fit the budget · `2` usage/environment error.

**Run layer, not the delivery board**: `mnemo card` is verified working against `graphos` (used to preview step cards before a run). Only `mnemo delivery` is never run against `graphos` — see AGENTS.md.
