# 09 — Merge

> `graphos merge <path-a> <path-b> -o <output-dir>`

Combine two knowledge graphs and re-cluster the result as a unified graph.

---

## Flow

```
┌──────────────────────────────────────────────────────────────┐
│                      MERGE FLOW                              │
│                                                              │
│  graphos merge graph-a/ graph-b/ -o merged/                 │
│       │                                                      │
│       ▼                                                      │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Load graph A (graphos-out/graph.json from path-a)    │  │
│  │  Load graph B (graphos-out/graph.json from path-b)    │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Merge graphs (Domain.Graph.mergeGraphs)             │  │
│  │  → Deduplicate nodes by NodeId                       │  │
│  │  → Merge edges (union, last-write on collision)     │  │
│  │  → Preserve directed flag from graph A               │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Re-cluster merged graph (Leiden)                    │  │
│  │  → Community IDs from A and B no longer valid       │  │
│  │  → Fresh detection on the combined graph            │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Infer edges (bridge edges, transitive deps)        │  │
│  │  → based on edge density setting                    │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Analyze (god nodes, bridges, surprises)             │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  Export merged graph to output-dir                           │
│  (graph.json, graph.html, GRAPH_REPORT.md)                 │
└──────────────────────────────────────────────────────────────┘
```

---

## Why This Workflow Exists

Two codebases analyzed separately produce two graphs with separate community structures. Merging them creates a unified view: cross-codebase dependencies emerge, shared modules connect, and new communities form that span both projects.

Use cases:
- Merging a microservice and its shared library
- Combining a frontend and backend codebase into one graph
- Unifying two versions of a project (before/after refactor)
- Aggregating multiple repositories into a single knowledge graph

---

## Key Behavior

- **Node deduplication**: Nodes with the same NodeId are merged (last-write wins from graph B)
- **Community invalidation**: Community IDs from the source graphs are discarded. The merged graph is re-clustered because combined edges change the optimal community structure
- **Directed flag**: The first graph's directed setting is preserved

---

## Prerequisite

Both input directories must contain a valid `graphos-out/graph.json` from a previous pipeline run.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--resolution N` | 1.0 | Leiden gamma for re-clustering |
| `--min-comm-size N` | 3 | Minimum community size |
| `--max-leiden-iterations N` | 50 | Max Leiden iterations |
| `--edge-density N` | 0.0 | Edge inference density |