---
name: mnemo-anchors
description: Add and check mnemo anchors — pointers from the memory graph into the code graphs of knowledge sources. Trigger when linking memory to code or verifying that anchors still resolve.
---

# mnemo anchors

Pointers from the memory graph into the code graphs of the knowledge sources.

```bash
mnemo anchors add   --project PROJECT <repo-relative paths>
mnemo anchors check
```

- **`add`** anchors the files that exist in a project of a knowledge source; a path that does not resolve is logged as an event.
- **`check`** re-resolves every anchor of a space, refreshes the cache, and marks stale the anchors that no longer resolve.
- `check` re-resolves a project in **one query capped at 2,000 rows** — keep the anchor set small (36 anchors resolve cleanly; the whole `src/` tree is reported falsely stale).
- For this repo: `mnemo-graphos anchors add --project Graphos <paths>` and `mnemo-graphos anchors check` (expect `stale anchors: 0`).
