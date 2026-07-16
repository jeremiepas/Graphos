# 06 — Explain

> `graphos explain <node>`

Show a node and all its direct connections — every neighbor, every edge, community membership.

---

## Flow

```
┌───────────────────────────────────────────────────────────┐
│                    EXPLAIN FLOW                            │
│                                                           │
│  graphos explain "RequestHandler"                        │
│       │                                                   │
│       ▼                                                   │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Load graph.json → LabeledGraph                     │ │
│  └────────────────────┬────────────────────────────────┘ │
│                       │                                   │
│                       ▼                                   │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Find node by label/id                              │ │
│  │  → full node details (kind, signature, location)   │ │
│  └────────────────────┬────────────────────────────────┘ │
│                       │                                   │
│                       ▼                                   │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Get all neighbors (Domain.Graph.neighbors)        │ │
│  │  → forward + backward adjacency                    │ │
│  └────────────────────┬────────────────────────────────┘ │
│                       │                                   │
│                       ▼                                   │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Get community membership                          │ │
│  │  → which community, cohesion, bridge status        │ │
│  └────────────────────┬────────────────────────────────┘ │
│                       │                                   │
│                       ▼                                   │
│  Output:                                               │
│    Node: RequestHandler                                │
│    Kind: Function                                      │
│    Community: 4 (Parser, cohesion: 0.72)               │
│    Is bridge: no                                       │
│    Degree: 12                                          │
│    Edges:                                              │
│      → AuthService [calls, EXTRACTED]                  │
│      → Router [depends_on, EXTRACTED]                  │
│      ← Config [references, EXTRACTED]                 │
│      ← Logger [imports, EXTRACTED]                     │
│      ...                                               │
└───────────────────────────────────────────────────────┘
```

---

## Why This Workflow Exists

Query and path give you subgraphs — subsets of nodes. But sometimes you need the **full picture** of one node: everything it connects to, what community it belongs to, whether it's a bridge. This is the "give me the neighborhood" workflow.

Use cases:
- Understanding what a function depends on and what depends on it
- Checking if a node is a bridge between communities
- Evaluating the importance of a node (by degree)
- Finding the community context for a piece of code

---

## Prerequisite

Requires an existing `graph.json`. Run the full pipeline first.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |