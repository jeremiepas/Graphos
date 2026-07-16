# 04 — Query

> `graphos query <question>`

Search an existing knowledge graph using BFS or DFS traversal with a token budget.

---

## Flow

```
┌───────────────────────────────────────────────────────────────┐
│                     QUERY FLOW                                │
│                                                               │
│  graphos query "how does authentication work?"               │
│       │                                                       │
│       ▼                                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Load graph.json (UseCase/Load)                        │  │
│  │  → LabeledGraph + GraphIndex (inverted index)          │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Normalize query terms (UseCase/Query/Normalize)       │  │
│  │  → lowercase, tokenize, filter short words             │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Match nodes (O(k×log N) via GraphIndex)              │  │
│  │  → find best-matching nodes by label/id                │  │
│  │  → take top 5 matches                                  │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│              ┌────────┴────────┐                              │
│              ▼                 ▼                              │
│  ┌─────────────────┐   ┌──────────────────┐                  │
│  │  BFS traversal  │   │  DFS traversal   │                  │
│  │  (default)      │   │  (--dfs)         │                  │
│  │                 │   │                  │                  │
│  │  Breadth-first: │   │  Depth-first:   │                  │
│  │  explore        │   │  explore deeper  │                  │
│  │  neighbors      │   │  along paths     │                  │
│  │  first          │   │  first           │                  │
│  └────────┬────────┘   └────────┬─────────┘                │
│           │                       │                           │
│           └───────────┬───────────┘                           │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Token budget enforcement                              │  │
│  │  → stop including nodes when budget exhausted          │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  QueryResult { nodes, edges, traverse }                      │
└───────────────────────────────────────────────────────────────┘
```

---

## Matching Algorithm

The query uses an **inverted index** (`Domain.Graph.Index`) for fast term lookup:

1. Query is tokenized and lowercased
2. Each token is looked up in the index (O(log N) per term)
3. Nodes scored by number of matching terms
4. Top 5 matching nodes become traversal starting points

This is O(k×log N + hits) instead of O(N) full-scan, making it 10–100x faster on large graphs.

---

## Traversal Modes

| Mode | Flag | Strategy | Best For |
|------|------|----------|----------|
| **BFS** | (default) | Explore neighbors outward level by level | Broad questions: "how does X relate to Y?" |
| **DFS** | `--dfs` | Follow paths deeper before backtracking | Deep questions: "what calls this function down the call chain?" |

Both modes respect the token budget — traversal stops when the allocated token count is exhausted.

---

## Token Budget

| Flag | Default | Purpose |
|------|---------|---------|
| `--budget N` | 2000 | Maximum tokens in the result |

The budget controls how much of the graph is included in the response. Higher budgets return more nodes and edges, giving broader context at higher token cost.

---

## Prerequisite

This workflow operates on an **existing** `graph.json`. Run the full pipeline first:

```
graphos .                    ← produces graph.json
graphos query "auth flow"    ← queries the produced graph
```

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--dfs` | bfs | Use DFS traversal |
| `--budget N` | 2000 | Token budget for results |
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |