---
name: mnemo-delivery
description: Operate the mnemo delivery layer — sync sources, import/render/check canvases, facts, where, next, run one step, record, backfill. Trigger for any delivery-board operation. Delivery spaces only, never graphos.
---

# mnemo delivery

The delivery layer ties a workflow to its external sources (Linear, dossier notes, TaskNotes, PRs, review comments, impact graph, OpenSpec changes) and to a canvas.

```bash
mnemo delivery sync [COMMAND]               # sync a read-only delivery source (all | linear | vault | impact | prs | …)
mnemo delivery import-canvas WF FILE [--force] [--adopt-done]   # read an Obsidian canvas into the graph
mnemo delivery render WF [--force]          # render the canvas of a workflow from the graph
mnemo delivery check WF [--fix]             # compare graph vs canvas vs ## Workflow of index.md
mnemo delivery facts WF                     # print the "Where it stands" paragraph (read-only, no model)
mnemo delivery where WF --auto|--text       # write "Where it stands" into the title node
mnemo delivery next WF [--as MODEL]         # JSON brief of the current step
mnemo delivery run WF                       # execute one step run (one attempt of one step)
mnemo delivery record WF STEP --summary TEXT [--evidence TEXT] [--sha SHA] [--pr N] [--counts TEXT]
mnemo delivery backfill WF --through STEP --summary TEXT
```

- **`next`** reads only and picks the exit code: `0` GO · `3` STOP · `4` HANDOFF · `1` ALL_DONE.
- **`run`** executes nothing unless the brief of `next` is GO, the workflow stores an executable delivery definition, and the step has an evidence predicate. A human step waits for `mnemo approve`.
- **`record`** marks one step done from an explicit manual record (no PR evidence required); it refuses a human step, a step already done, and a step that is a problem.
- **`backfill`** marks unfinished steps up to `--through` done, only on linked-PR evidence (a PR from the pull-request step, a CAN MERGE/IMPROVEMENT verdict on every open PR, every PR merged at the human-merge step). Runs only when invoked.

**Hard rule**: this repo (`graphos`) has no delivery section — never run any `mnemo delivery` command against `graphos`. Delivery is `homelab` / `solario` work.
