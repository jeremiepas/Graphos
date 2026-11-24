---
name: mnemo-consolidate
description: Prune learned links and merge step episodes into facts in the mnemo memory graph. Trigger on memory-graph maintenance, never during a run.
---

# mnemo consolidate

```bash
mnemo consolidate [--dry-run] [--space SPACE]
```

- Prunes the learned links that are under the threshold or beyond the maximum of a node, and merges the episodes of each step that has too many into one fact written by a model.
- Episodes are **archived, never deleted**.
- Never done by a run. `--dry-run` reports what would change without writing.
