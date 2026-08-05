# 04 — Query

> `graphos query <question>`

Search an existing knowledge graph using scored traversal with match verdict, did-you-mean suggestions, and result-set hashing.

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
│  │  Scored term matching (O(k×log N) via GraphIndex)      │  │
│  │  → normalized score = matched-terms ÷ query-terms      │  │
│  │  → verdict: strong (≥0.5) | weak (>0, <0.5) | none (0)│  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│              ┌────────┴────────┐                              │
│              ▼                 ▼                              │
│  ┌─────────────────┐   ┌──────────────────┐                  │
│  │  strong/weak    │   │  none (NoMatch)   │                  │
│  │  BFS traversal │   │  no traversal     │                  │
│  │  scored results │   │  did-you-mean     │                  │
│  │  + hash         │   │  suggestions      │                  │
│  └────────┬────────┘   └────────┬─────────┘                │
│           │                       │                           │
│           └───────────┬───────────┘                           │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Refine pass (noise control)                           │  │
│  │  → semantic edge filter (--edges semantic|all)         │  │
│  │  → self-edge collapse                                  │  │
│  │  → duplicate declaration dedup                         │  │
│  │  → label elision (--label-width)                       │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Budget-aware rendering                               │  │
│  │  → verdict header + per-node scores + hash            │  │
│  │  → relevance-descending output                        │  │
│  │  → tail truncation with omission footer               │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       ▼                                       │
│  QueryResponse { verdict, bestScore, hash, nodes, edges,    │
│                  suggestions }                                 │
└───────────────────────────────────────────────────────────────┘
```

---

## Matching Algorithm

The query uses an **inverted index** (`Domain.Graph.Index`) for fast term lookup:

1. Query is tokenized and lowercased
2. Each token is looked up in the index (O(log N) per term)
3. Nodes scored by normalized match count (matched-terms ÷ query-terms)
4. Exact full-label matches get a small boost
5. Verdict computed from best score: strong ≥ 0.5, weak > 0, none = 0

**No fabricated results**: When verdict is `none`, no traversal occurs — only did-you-mean suggestions are returned.

---

## Verdict System

| Verdict | Best Score | Behavior |
|--------|-----------|----------|
| **strong** | ≥ 0.5 | Full scored results with BFS expansion |
| **weak** | > 0, < 0.5 | Results returned alongside suggestions |
| **none** | 0 | No traversal, no results — only suggestions |

---

## Result-Set Hash

Every response includes a short hash over the ordered result node ids. Identical query → identical hash → caller can detect "no new information".

---

## Noise Controls

| Flag | Default | Description |
|------|---------|-------------|
| `--edges semantic\|all` | `semantic` | Filter structural edges to trivia targets (undefined, null, Promise, etc.) |
| `--label-width N` | 120 | Elide long labels at word boundary with `...` |

---

## Path Scoping

| Flag | Description |
|------|-------------|
| `--path <glob>` | Restrict results to nodes whose source file matches the glob pattern (e.g. `src/cli/**`) |

Path-like query terms (containing `/`) also consult the path index, so `graphos query "src/cli/commands"` matches nodes under that directory.

---

## Output Format

### Text Mode (default)

```
Verdict: strong (best score: 0.85) [hash: a3f29c01]

Results (3 nodes):
  0.85  AuthModule [auth-mod-001] (src/auth/AuthModule.hs)
  0.42  AuthHandler [auth-hnd-002] (src/auth/AuthHandler.hs)
  0.21  Database [db-001] (src/db/Database.hs)

Connections:
  AuthModule --imports--> AuthHandler [1.0]
  AuthModule --calls--> Database [0.8]
```

### JSON Mode (`--json`)

```json
{
  "verdict": "strong",
  "bestScore": 0.85,
  "hash": "a3f29c01",
  "nodes": [...],
  "edges": [...],
  "suggestions": []
}
```

---

## Traversal Modes

| Mode | Flag | Strategy | Best For |
|------|------|----------|----------|
| **BFS** | (default) | Explore neighbors outward level by level | Broad questions |
| **DFS** | `--dfs` | Follow paths deeper before backtracking | Deep call chains |

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--dfs` | bfs | Use DFS traversal |
| `--budget N` | 2000 | Token budget for results |
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |
| `--json` | off | Output as JSON |
| `--edges semantic\|all` | semantic | Edge filtering mode |
| `--label-width N` | 120 | Max label width before elision |
| `--path <glob>` | (none) | Restrict to source file paths matching glob |

---

## Prerequisite

This workflow operates on an **existing** `graph.json`. Run the full pipeline first:

```
graphos .                    ← produces graph.json
graphos query "auth flow"    ← queries the produced graph
```