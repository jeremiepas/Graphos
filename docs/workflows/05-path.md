# 05 — Path

> `graphos path <from> <to>`

Find the shortest path between two nodes in the knowledge graph using BFS.

---

## Flow

```
┌───────────────────────────────────────────────────────────┐
│                     PATH FLOW                            │
│                                                           │
│  graphos path "AuthModule" "Database"                    │
│       │                                                   │
│       ▼                                                   │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Load graph.json → LabeledGraph + GraphIndex       │ │
│  └────────────────────┬──────────────────────────────┘ │
│                       │                                 │
│                       ▼                                 │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  Match source and target nodes by label/id          │ │
│  │  (O(k×log N) via inverted index)                    │ │
│  └────────────────────┬──────────────────────────────┘ │
│                       │                                 │
│                       ▼                                 │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  BFS shortest path (Domain.Graph.shortestPath)    │ │
│  │  → returns ordered list of NodeIds                 │ │
│  └────────────────────┬──────────────────────────────┘ │
│                       │                                 │
│                       ▼                                 │
│  Path result: [Auth, AuthMiddleware, DBPool, Database]  │
│  Each hop shows: node label, relation, confidence      │
└───────────────────────────────────────────────────────┘
```

---

## Algorithm

BFS guarantees the shortest path in an unweighted graph. The implementation uses FGL's `esp` (shortest path) function on the graph converted via the FGL adapter.

For directed graphs (`--directed`), paths follow edge direction. For undirected graphs (default), edges are traversable in both directions.

---

## Output

The result is a list of nodes forming the shortest path, each with:
- Node label (function name, class name, etc.)
- Edge relation to the next node
- Confidence score (how certain the edge is)

If no path exists, a "no path found" message is returned.

---

## Prerequisite

Requires an existing `graph.json`. Run the full pipeline first.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |