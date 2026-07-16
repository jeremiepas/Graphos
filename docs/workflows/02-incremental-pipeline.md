# 02 — Incremental Pipeline

> `graphos <path> --update`

Re-runs the pipeline on an existing graph, only re-extracting files that changed since the last run.

---

## Why This Workflow Exists

Full extraction is expensive — it spawns LSP servers, parses every file, and rebuilds the entire graph. On a large codebase (100k+ files), a full run can take minutes. Incremental mode re-extracts only what changed, reducing a re-run from minutes to seconds.

---

## Flow

```
┌──────────────────────────────────────────────────────────────────┐
│                   INCREMENTAL PIPELINE                            │
│                                                                  │
│  1. Load graphos-out/cache/graph.json (previous result)          │
│  2. Load graphos-out/cache/ SHA256 hashes                        │
│  3. Detect files — compare current SHA256 vs cached              │
│  4. Split into:                                                 │
│     ┌──────────────────┐   ┌──────────────────┐               │
│     │   Changed files   │   │  Unchanged files │               │
│     │   (re-extract)   │   │  (reuse cache)   │               │
│     └────────┬─────────┘   └────────┬─────────┘               │
│              │                       │                           │
│              ▼                       │                           │
│     Extract via LSP/TS/LLM          │                           │
│              │                       │                           │
│              └──────────┬───────────┘                           │
│                         ▼                                       │
│              Merge: old extractions (unchanged)                  │
│                    + new extractions (changed)                    │
│                         │                                       │
│                         ▼                                       │
│              Build → Cluster → Infer → Analyze → Export         │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## SHA256 Cache

Every file gets a SHA256 hash on first extraction. The mapping is stored in `graphos-out/cache/`:

```
graphos-out/cache/
├── graph.checkpoint.json    ← pipeline state after Build stage
├── <hash>.sha256            ← per-file hash records
└── ...
```

On `--update`:
- Current file hashes are computed
- Compared against cached hashes
- Only files with changed hashes are re-extracted
- Unchanged files reuse their cached extraction results

---

## Checkpoint Resume

If a previous pipeline run was interrupted (crash, Ctrl+C, OOM), the checkpoint allows resuming from the last completed stage:

| Checkpoint | Resume From |
|-----------|-------------|
| After Detect | Re-run from Extract |
| After Extract | Re-run from Build |
| After Build | Re-run from Cluster (graph already constructed) |
| After Cluster | Re-run from Infer + Analyze + Export |
| After Export | Full run complete, no checkpoint present |

The checkpoint is stored at `graphos-out/cache/graph.checkpoint.json` and removed after successful export.

---

## When to Use

| Scenario | Full Run | Incremental |
|----------|----------|-------------|
| First run on a codebase | Yes | No |
| After editing a few files | No | Yes |
| After adding/removing files | No | Yes |
| After LSP server update | Yes | No (extraction may differ) |
| After Graphos version update | Yes | No (format may differ) |
| Periodic full rebuild | Yes (weekly) | No |

---

## Trade-offs

| Aspect | Full Run | Incremental |
|--------|----------|-------------|
| Speed | Slow (all files) | Fast (changed files only) |
| Completeness | 100% fresh | Depends on cache accuracy |
| Disk usage | Overwrites previous | Merges with previous |
| Correctness risk | None | Stale cache if external tools changed |

---

## Configuration

No special flags beyond `--update`. All standard pipeline flags (`--resolution`, `--directed`, etc.) apply to the re-cluster and re-export stages.