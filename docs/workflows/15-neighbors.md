# 15 — Neighbors

> `graphos neighbors <node-id> [--depth N]`

Cheap foothold expansion from a known node — explore the neighborhood without re-entering fuzzy search.

---

## Flow

```
┌───────────────────────────────────────────────────────────────┐
│                     NEIGHBORS FLOW                            │
│                                                               │
│  graphos neighbors auth-mod-001 --depth 2                    │
│       │                                                       │
│       ▼                                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Load graph.json → LabeledGraph + GraphIndex          │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Exact node ID lookup                                 │  │
│  │  → BFS to depth N (default 2) over adjacency          │  │
│  │  → proximity score = 1/(1+hops)                      │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│              ┌────────┴────────┐                              │
│              ▼                 ▼                              │
│  ┌─────────────────┐   ┌──────────────────┐                  │
│  │  Found          │   │  Not Found         │                  │
│  │  → scored nodes│   │  → "Node not found"│                  │
│  │  → edges        │   │                     │                  │
│  │  → proximity    │   │                     │                  │
│  └─────────────────┘   └───────────────────┘                  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Refine pass (noise control)                           │  │
│  │  → semantic edge filter, self-edge collapse            │  │
│  │  → declaration dedup, label elision                   │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       ▼                                       │
│  NeighborsResult { centerNode, nodes, edges, maxDepth }      │
└───────────────────────────────────────────────────────────────┘
```

---

## Semantics

- **Node ID, not fuzzy term**: The argument must be an exact node ID (e.g., `auth-mod-001`), not a search term. Use `graphos symbols` or `graphos query` to discover node IDs.
- **BFS expansion**: Expands from the start node to `--depth` hops (default 2). Direct neighbors are hop-1, their neighbors are hop-2, etc.
- **Proximity scoring**: Nodes are scored by proximity: `1/(1+hops)`. Hop-1 nodes appear before hop-2 nodes.
- **Noise controls**: The same `--edges` and `--label-width` options apply to neighbors output.

---

## Output

### Text Mode (default)

```
Neighbors of auth-mod-001 (depth 2):

0.50  AuthHandler [auth-hnd-002] (src/auth/AuthHandler.hs)
0.33  Database [db-001] (src/db/Database.hs)
0.33  Router [rtr-001] (src/http/Router.hs)

Connections:
  AuthModule --imports--> AuthHandler [1.0]
  AuthModule --calls--> Database [0.8]
```

### JSON Mode (`--json`)

```json
{
  "center_node": "auth-mod-001",
  "nodes": [...],
  "edges": [...],
  "max_depth": 2
}
```

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--depth N` | 2 | BFS depth (number of hops) |
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |
| `--budget N` | 2000 | Token budget for output |
| `--json` | off | Output as JSON |
| `--label-width N` | 120 | Max label width before elision |
| `--edges semantic\|all` | semantic | Edge filtering mode |

---

## Prerequisite

Requires an existing `graph.json`. Run the full pipeline first.

Use `graphos symbols <name>` or `graphos query <term>` first to discover node IDs, then use `graphos neighbors <id>` to expand from a known-good node.