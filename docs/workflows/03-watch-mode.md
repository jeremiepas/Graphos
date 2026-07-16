# 03 — Watch Mode

> `graphos <path> --watch`

Continuously watches a directory for file changes and auto-runs the incremental pipeline when changes are detected.

---

## Flow

```
┌──────────────────────────────────────────────────────────────┐
│                    WATCH MODE                                │
│                                                              │
│  1. Run full pipeline initially                             │
│     detect → extract → build → cluster → infer → analyze → export
│                                                              │
│  2. Enter watch loop                                        │
│     ┌────────────────────────────────────────────────────┐  │
│  │  │  Infrastructure/FileSystem/Watcher.hs              │  │
│  │  │                                                    │  │
│  │  │  Watch for filesystem events (inotify/poll)      │  │
│  │  │  Debounce (avoid re-triggering on rapid saves)   │  │
│  │  │  Filter: respect .gitignore + sensitive files    │  │
│  │  └─────────────────┬──────────────────────────────┘  │
│                        │                                     │
│                        ▼                                     │
│     ┌────────────────────────────────────────────────────┐  │
│  │  │  Run incremental pipeline on changed files       │  │
│  │  │                                                    │  │
│  │  │  1. Detect which files changed                    │  │
│  │  │  2. Re-extract changed files only                │  │
│  │  │  3. Merge into existing graph                    │  │
│  │  │  4. Re-cluster entire graph                      │  │
│  │  │  5. Incremental export (append to JSON, rebuild HTML)
│  │  └────────────────────────────────────────────────────┘  │
│                                                              │
│  3. Return to watch loop                                    │
│                                                              │
│  Ctrl+C to stop                                             │
└──────────────────────────────────────────────────────────────┘
```

---

## Behavior

1. **Initial run**: A full pipeline executes first, producing `graph.json`, `graph.html`, and `GRAPH_REPORT.md`
2. **File change detected**: The watcher identifies which files changed
3. **Incremental re-extraction**: Only changed files are re-extracted via their respective extractors (LSP, tree-sitter, LLM, etc.)
4. **Re-merge + Re-cluster**: The graph is rebuilt and Leiden clustering runs on the full graph (clustering is fast enough for this)
5. **Incremental export**: Results are appended to `graph.json` and HTML is rebuilt
6. **Loop**: Returns to watching

---

## Debouncing

Filesystem events can fire rapidly (editor auto-save, `git checkout`, bulk file moves). The watcher debounces events to avoid triggering the pipeline multiple times for a single logical change. The exact debounce window is tuned for interactive editing patterns.

---

## .gitignore Awareness

The watcher respects `.gitignore` patterns. Files matching ignored patterns are not watched, preventing unnecessary re-runs on build artifacts, dependencies, and generated files.

---

## When to Use

| Scenario | Use Watch Mode |
|----------|----------------|
| Active development on a codebase | Yes — see graph update live |
| CI/CD integration | No — use full or incremental pipeline |
| Periodic bulk commits | No — use incremental |
| Exploring a foreign codebase | Maybe — useful for live feedback |
| Monitoring a production service | No — not the right tool |

---

## Interaction with Other Flags

| Flag | Behavior |
|------|----------|
| `--directed` | Preserved across incremental updates |
| `--no-viz` | HTML is never generated |
| `--no-cluster` | Clustering skipped on every re-run |
| `--resolution N` | Applied on every re-cluster |
| `--neo4j` | Neo4j push runs after each incremental update |
| `--otel` | Tracing/metrics active for the entire watch session |

---

## Lifecycle

```
Start:   graphos <path> --watch
               │
               ▼
         Full pipeline (initial)
               │
         ┌─────▼──────┐
         │  Watching   │◀────────────────────┐
         │  for change │                      │
         └─────┬──────┘                      │
               │ file changed                │
               ▼                              │
         Incremental pipeline               │
               │                              │
               ▼                              │
         Export updated outputs ─────────────┘
               
Stop:    Ctrl+C → clean shutdown
```

Watch mode runs until the user stops it (Ctrl+C). There is no time limit or automatic stop condition.