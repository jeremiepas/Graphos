# Brainstorm: Graphos as LLM Context Optimizer

## Core Idea

Use the knowledge graph as a **semantic filter** between user exchanges and LLMs:
- **Small local LLM** (fast, no thinking) → selects relevant subgraph → builds minimal context
- **Big LLM** (slow, thinking mode) → receives only what matters → deep analysis

The graph acts as a **context compressor**: instead of sending raw files, send only the relevant nodes, edges, and communities.

---

## Problem

| Pain Point | Description |
|---|---|
| Token overflow | Large codebases exceed context windows |
| Noisy context | Irrelevant code dilutes LLM reasoning |
| Cost | Big models charge per token, most are wasted |
| Latency | More tokens = slower inference |
| Repetition | Same context re-sent across exchanges |

---

## Architecture: Two-Tier LLM Pipeline

```
User Query
    │
    ▼
┌─────────────────────────┐
│  TIER 1: Small Local LLM│  (e.g. Phi-3, Llama-3.2-1B, Gemma-2B)
│  Role: Context Router     │
│  Mode: No thinking         │
│  Speed: <500ms             │
└──────────┬──────────────┘
           │ 1. Normalize query
           │ 2. Query graph for relevant subgraph
           │ 3. Score nodes/edges by relevance
           │ 4. Select context window budget
           ▼
┌─────────────────────────┐
│  Graphos Knowledge Graph │
│  - Query graph (BFS/DFS)│
│  - Get communities       │
│  - Find god nodes        │
│  - Shortest paths        │
│  - Bridge nodes          │
└──────────┬──────────────┘
           │ 5. Extracted subgraph
           │ (nodes, edges, community labels)
           ▼
┌─────────────────────────┐
│  TIER 2: Big LLM         │  (e.g. Claude, GPT-4, Llama-70B)
│  Role: Deep Reasoning     │
│  Mode: Thinking enabled   │
│  Input: Minimal context   │
│  Output: Rich response     │
└──────────┬──────────────┘
           │
           ▼
    Response to User
           │
           ▼
┌─────────────────────────┐
│  Update Graph (optional)  │
│  - Add new exchange node  │
│  - Link to relevant nodes │
│  - Update edge weights    │
└─────────────────────────┘
```

---

## Graph-Based Context Selection Strategies

### Strategy 1: Community-Aware Selection

The graph has community detection (Leiden algorithm). Each community represents a cohesive module.

```
1. Map user query → find best matching node (query_graph)
2. Get that node's community (get_community)
3. Include all nodes from that community
4. Include bridge nodes connecting to adjacent communities
5. Exclude all other communities

Result: ~5-15% of total graph instead of 100%
```

**Token savings**: 80-95% reduction for focused queries.

### Strategy 2: Relevance-Weighted BFS

```
1. Start from best-matching node
2. BFS with token budget
3. Score each visited node:
   - Label similarity to query: +3
   - Same community as start: +2
   - Edge confidence = EXTRACTED: +2
   - Edge confidence = INFERRED: +1
   - Edge confidence = AMBIGUOUS: +0
   - Bridge node: +2
   - God node (high degree): +1
4. Sort by score, include top-N within budget
```

### Strategy 3: Path-Based Selection (Cross-Module Queries)

When the query spans multiple modules:

```
1. Find two best-matching nodes (source, target)
2. Compute shortest_path between them
3. Include: all nodes on the path + their immediate neighbors
4. Include: communities of path nodes
5. Token budget determines neighbor depth
```

### Strategy 4: Differential Context (Conversation Memory)

Across multiple exchanges in the same conversation:

```
Exchange 1: Include community A + bridge nodes (1000 tokens of graph context)
Exchange 2: Query matches community A → reuse context from exchange 1
           + expand to community B via bridge nodes (+300 tokens)
Exchange 3: Topic shifted to community C
           → Drop community A context
           → Keep community B bridge nodes (they connect C to previous context)
           → Add community C

Graph tracks conversation history as nodes themselves.
```

---

## Small LLM Context Router: Design

### Input
- User query (raw text)
- Graph metadata (community labels, god node labels — NOT full graph)

### Algorithm
```
function select_context(query, graph, budget_tokens):
    # Phase 1: Match (using normalizeForSearch)
    start_nodes = find_best_nodes(query, graph)
    
    # Phase 2: Explore (BFS/DFS within budget)
    subgraph = query_graph(query, mode="bfs", budget=budget_tokens)
    
    # Phase 3: Score & Prune
    scored = [(node, relevance_score(node, query)) for node in subgraph]
    scored.sort(by_score_desc)
    
    # Phase 4: Budget Allocation  
    context = ""
    tokens_used = 0
    for node, score in scored:
        snippet = format_node_for_llm(node)  # compact representation
        if tokens_used + len(snippet) <= budget_tokens:
            context += snippet
            tokens_used += len(snippet)
    
    # Phase 5: Enrich with structural info
    context += format_communities(relevant_communities)
    context += format_bridge_nodes(relevant_bridges)
    context += format_god_nodes(top_god_nodes)
    
    return context
```

### Output Format (Compact for Big LLM)
```
## Relevant Code Graph (community: "LSP Client", 42 nodes, 89 edges)

### Key Nodes
- Graphos.Infrastructure.LSP.Client [god-node, degree=292]
  → connects: LSP.Protocol, LSP.Capabilities, Domain.Types
- connectToLSP [function] — src: src/LSP/Client.hs:45
- extractDocumentSymbols [function] — src: src/LSP/Client.hs:112

### Key Edges  
- Client → Protocol [imports, EXTRACTED]
- Client → Domain.Types [imports, EXTRACTED]
- Client → Capabilities [depends_on, INFERRED]

### Bridge Nodes (connects to other communities)
- Domain.Types → also in communities: [Types, Analysis, Community]

### Suggested Context Expansion
- If reasoning about LSP protocol: include Protocol community (28 nodes)
- If reasoning about types: include Types community (15 nodes)
```

This is FAR more compact than sending the full source files.

---

## Conversation Graph: Persisting Exchange History

### Concept
Each user-LLM exchange becomes a node in the graph:

```
Node: exchange_2026_04_17_001
  label: "How does MCP server handle tool calls?"
  file_type: conversation
  edges:
    → Graphos.Infrastructure.Server.MCP [references]
    → handleToolCall [contains]
    → previous_exchange [conceptually_related_to]
```

### Benefits
- Small LLM can query "what did we discuss before?" via graph traversal
- No need to keep full conversation in context — just paths through the graph
- Cross-session memory: conversations become persistent knowledge
- The graph learns which topics are frequently discussed together

### Schema Extension
```haskell
data ConversationNode = ConversationNode
  { convId        :: Text          -- exchange ID
  , convQuestion  :: Text          -- user's question (normalized)
  , convSummary   :: Text          -- LLM's response summary (generated by small LLM)
  , convTimestamp :: UTCTime
  , convRelevantNodes :: [NodeId]   -- nodes referenced in this exchange
  , convTokensUsed    :: Int        -- token cost of this exchange
  }
```

---

## Token Budget Allocation

### Dynamic Budget Based on Query Complexity

| Query Type | Graph Context | Source Code | Headroom |
|---|---|---|---|
| Focused (single function) | 500 tokens | 2000 tokens | 75% |
| Module-level (one community) | 1500 tokens | 4000 tokens | 55% |
| Cross-module (path query) | 2500 tokens | 3000 tokens | 55% |
| Architectural (god nodes + bridges) | 3000 tokens | 1000 tokens | 70% |
| Exploratory (broad) | 2000 tokens | 2000 tokens | 65% |

### Budget Formula
```
graph_context_tokens = min(
  total_budget * graph_ratio,
  nodes * avg_tokens_per_node    -- compact: ~50 tokens per node
  + edges * avg_tokens_per_edge  -- compact: ~20 tokens per edge  
  + communities * 100           -- label + stats per community
)

where graph_ratio varies by query complexity (0.1 - 0.4)
```

---

## Implementation Phases

### Phase 1: Context Selector (Small LLM + Graph)
- [ ] Implement `select_context` function using existing graph query tools
- [ ] Add token counting/budgeting
- [ ] Test with Ollama (local) + Claude (remote)
- [ ] Measure token reduction vs full-context baseline

### Phase 2: Conversation Memory
- [ ] Add conversation nodes to graph after each exchange
- [ ] Link conversations to relevant code nodes
- [ ] Implement "what did we discuss about X?" queries
- [ ] Test cross-session memory persistence

### Phase 3: Smart Router
- [ ] Small LLM classifies query complexity (focused/module/cross/architecture)
- [ ] Adjusts graph context budget accordingly
- [ ] Decides when big LLM thinking mode is needed vs when small LLM suffices
- [ ] Implements local-first: small LLM answers simple questions directly

### Phase 4: Adaptive Learning
- [ ] Track which context selections led to good LLM responses
- [ ] Adjust edge weights based on retrieval success
- [ ] Community labels improve as more conversations link to nodes
- [ ] Small LLM improves routing via fine-tuning on conversation logs

---

## Local LLM Candidates

| Model | Params | RAM | Speed | Use |
|---|---|---|---|---|
| Phi-3-mini | 3.8B | 4GB | Fast | Context router, simple Q&A |
| Llama-3.2-1B | 1B | 2GB | Very fast | Query normalization, node scoring |
| Gemma-2-2B | 2B | 3GB | Fast | Community matching, summarization |
| Qwen2.5-Coder-1.5B | 1.5B | 2GB | Fast | Code-aware routing |
| DeepSeek-Coder-1.3B | 1.3B | 2GB | Fast | Code-aware routing |

All runnable on CPU via Ollama/lmstudio.

---

## Key Metrics to Track

1. **Token Reduction Ratio**: `full_context_tokens / graph_optimized_tokens`
2. **Response Quality**: A/B test with/without graph context
3. **Retrieval Precision**: % of relevant nodes included in subgraph
4. **Routing Accuracy**: % of queries correctly classified by small LLM
5. **Cost Savings**: $/exchange with and without optimization
6. **Latency**: End-to-end time with and without routing

---

## Risks & Mitigations

| Risk | Mitigation |
|---|---|
| Small LLM misses relevant context | Fallback: include bridge nodes + god nodes always |
| Graph is stale (code changed) | Git hook auto-rebuilds (already exists in Graphos) |
| Over-compression loses nuance | Budget minimum: always include source file for top-3 nodes |
| Small LLM routing errors | Confidence threshold: if unsure, send more context |
| Community boundaries wrong | Adjustable resolution parameter + merge small communities |

---

## Future: Graph as Persistent Memory for AI Coding Agents

The ultimate vision:

```
┌──────────┐     ┌──────────┐     ┌──────────┐
│  Small LLM │────▶│  Graphos  │◀────│  Big LLM  │
│  (local)   │     │  (graph)  │     │  (cloud)  │
└──────────┘     └────┬─────┘     └──────────┘
                      │
                 ┌────▼─────┐
                 │  Codebase │
                 │  + History│
                 │  + Chats  │
                 └──────────┘

The graph becomes the agent's long-term memory.
Every exchange, every decision, every insight — persisted as nodes and edges.
The small LLM navigates this memory to build minimal, high-signal context.
The big LLM thinks deeply with exactly the right context.
```

This is essentially a **RAG system where the retrieval index is a knowledge graph with community structure**, rather than flat vector embeddings. The graph structure (communities, bridges, paths) provides much richer retrieval signals than similarity search alone.