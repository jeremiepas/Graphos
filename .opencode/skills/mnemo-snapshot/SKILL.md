---
name: mnemo-snapshot
description: Export or restore a mnemo space snapshot. Trigger when backing up a space to a file or restoring a snapshot into an empty space.
---

# mnemo snapshot (export / import)

```bash
mnemo export FILE [--space SPACE]   # write every node + relationship of a space to a file
mnemo import FILE                   # restore a snapshot into its space, which must be empty
```

- **`export`** writes every node and relationship of a space to `FILE`.
- **`import`** restores a snapshot into its space, which must be empty; it restores into the space recorded in the snapshot.
- These are whole-space operations — use them for backup/migration, not for per-item changes.
