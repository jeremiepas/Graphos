# 07 — Context Selection

> `graphos query <question>` (via MCP `select_context`)

Select a minimal, high-signal subgraph from the knowledge graph for LLM consumption. This is the core of Graphos's value proposition: instead of sending raw files to an LLM, send only what matters.

---

## Why This Workflow Exists

LLMs have limited context windows. Sending an entire codebase wastes tokens on irrelevant code. Context selection acts as a **context compressor**: given a question, it finds the smallest subgraph that answers it.

```
 WITHOUT context selection              WITH context selection
 ┌─────────────────┐                   ┌─────────────────┐
 │  Entire codebase │                   │  Relevant       │
 │  500k tokens    │                   │  subgraph       │
 │                 │                   │  1.5k tokens   │
 │  Most tokens   │                   │  Every token   │
 │  wasted on     │                   │  relevant      │
 │  irrelevant    │                   │                 │
 │  code          │                   │                 │
 └─────────────────┘                   └─────────────────┘
```

---

## Flow

```
┌──────────────────────────────────────────────────────────────────┐
│               CONTEXT SELECTION PIPELINE                         │
│                                                                  │
│  User Question                                                   │
│      │                                                           │
│      ▼                                                           │
│  ┌─────────────────────────┐                                     │
│  │  COMPLEXITY CLASSIFIER  │                                     │
│  │  (UseCase/SelectContext)│                                     │
│  │                         │                                     │
│  │  Analyzes: number of    │                                     │
│  │  terms, specificity,   │                                     │
│  │  scope breadth          │                                     │
│  └──────────┬──────────────┘                                     │
│              │                                                    │
│              ▼                                                    │
│  ┌────────────────────────────────────────────────────────┐      │
│  │  CLASSIFICATION RESULT                                │      │
│  │                                                        │      │
│  │  Focused       → single function/class                │      │
│  │  ModuleLevel  → one community                        │      │
│  │  CrossModule   → spans multiple communities           │      │
│  │  Architectural → system-wide structure                 │      │
│  │  Exploratory   → broad, unclear scope                  │      │
│  └──────────┬────────────────────────────────────────────┘      │
│             │                                                    │
│             ▼                                                    │
│  ┌────────────────────────────────────────────────────────┐      │
│  │  STRATEGY SELECTION                                  │      │
│  │                                                      │      │
│  │  Focused/Module  → Community-aware                   │      │
│  │  CrossModule     → Path-based                        │      │
│  │  Architectural  → God nodes + bridges + structure     │      │
│  │  Exploratory     → Relevance-weighted BFS             │      │
│  └──────────┬───────────────────────────────────────────┘      │
│             │                                                    │
│             ▼                                                    │
│  ┌─────────────────────────┐                                    │
│  │  BUDGET ALLOCATION     │                                     │
│  │                         │                                     │
│  │  Focused:     500 tok   │                                     │
│  │  Module:    1500 tok   │                                     │
│  │  CrossMod:  2500 tok   │                                     │
│  │  Architect: 3000 tok   │                                     │
│  │  Exploratory: 2000 tok │                                     │
│  └──────────┬──────────────┘                                    │
│              │                                                    │
│              ▼                                                    │
│  ┌─────────────────────────────────────────────────────┐        │
│  │  COMPACT MARKDOWN OUTPUT                            │        │
│  │                                                     │        │
│  │  Each node: id, kind, signature, line range        │        │
│  │  Each edge: from → to, relation, confidence        │        │
│  │  Communities: label, size, cohesion               │        │
│  │  Bridge nodes flagged                              │        │
│  └─────────────────────────────────────────────────────┘        │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## Five Selection Strategies

### Strategy 1: Community-Aware (Focused / Module-Level)

Best for: "How does the parser work?", "What does AuthModule do?"

```
1. Find best-matching node in graph
2. Get that node's community
3. Include ALL nodes from that community
4. Include bridge nodes connecting to adjacent communities
5. Exclude all other communities
```

Token savings: 80–95% for focused queries.

### Strategy 2: Path-Based (Cross-Module)

Best for: "How does authentication connect to the database?"

```
1. Find two best-matching nodes (source, target)
2. Compute shortest_path between them
3. Include: all nodes on the path + their immediate neighbors
4. Include: communities of path nodes
5. Budget determines neighbor depth
```

### Strategy 3: Architectural (System-Wide)

Best for: "What are the main modules and how do they connect?"

```
1. Include all god nodes (highest-degree hubs)
2. Include all bridge nodes (articulation points)
3. Include community structure (labels, sizes, cohesion)
4. Include inter-community edges
5. Minimal per-node detail — focus on structure
```

### Strategy 4: Relevance-Weighted BFS (Exploratory)

Best for: "Tell me about error handling" (broad, unclear scope)

```
1. Start from best-matching nodes
2. BFS with token budget
3. Score each visited node:
   - Label similarity to query: +3
   - Same community as start: +2
   - Edge confidence EXTRACTED: +2
   - Edge confidence INFERRED: +1
   - Bridge node: +2
   - God node (high degree): +1
4. Sort by score, include top-N within budget
```

### Strategy 5: Differential (Cross-Session Memory)

Across multiple exchanges in the same conversation:

```
Exchange 1: Include community A + bridges (1000 tokens)
Exchange 2: Query matches community A → reuse context
           + expand to community B via bridges (+300 tokens)
Exchange 3: Topic shifted to community C
           → Drop community A context
           → Keep community B bridges
           → Add community C
```

---

## Budget Allocation

| Query Type | Graph Context | Source Code | Headroom |
|-----------|--------------|-------------|----------|
| Focused | 500 tokens | 2000 tokens | 75% |
| Module-level | 1500 tokens | 4000 tokens | 55% |
| Cross-module | 2500 tokens | 3000 tokens | 55% |
| Architectural | 3000 tokens | 1000 tokens | 70% |
| Exploratory | 2000 tokens | 2000 tokens | 65% |

---

## Chat History Filtering

Context selection excludes the synthetic chat community (community 0) by default. This prevents conversation memory from polluting code structure context. Opt in with `include_history=true` to include past conversation nodes.

---

## Prerequisite

Requires an existing `graph.json` with community assignments. Run the full pipeline first.