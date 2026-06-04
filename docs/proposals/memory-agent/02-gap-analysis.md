# 02 — Gap Analysis

What's missing for Graphos to become a full memory agent, ranked by priority and effort.

## Priority Classification

| Priority | Meaning |
|----------|---------|
| 🔴 Critical | Blocks core memory agent functionality |
| 🟠 High | Severely limits usefulness |
| 🟡 Medium | Reduces quality of memory retrieval |
| 🟢 Low | Nice-to-have optimization |

## Gap 1: Real-Time Graph Mutation 🔴 Critical

**Problem**: The MCP server loads the graph once at startup. `add_conversation` saves conversations to disk but does NOT update the in-memory `Graph` value. New conversations are invisible until restart.

**Impact**: An LLM agent can store memories, but cannot retrieve them within the same session. This defeats the purpose of cross-turn memory.

**Current flow**:
```
startup → load graph.json + memory/ → immutable Graph
add_conversation → saves to disk → in-memory Graph unchanged
conversation_history → searches in-memory Graph → misses new conversations
```

**Needed flow**:
```
startup → load graph.json + memory/ → mutable Graph (TVar/IORef)
add_conversation → saves to disk + updates in-memory Graph → visible immediately
conversation_history → searches updated Graph → finds new conversations
```

**Effort**: 2-3 days

**Implementation**:
- Change `startMCPServer` to use `TVar Graph` or `IORef Graph`
- `add_conversation` updates the TVar after saving to disk
- `select_context` and `conversation_history` read from the TVar
- Periodic or triggered write-back to `graph.json`

---

## Gap 2: Semantic Search / Embedding-Based Retrieval 🔴 Critical

**Problem**: All query matching is substring-based (`T.isInfixOf`). An agent asking "how does authentication work" won't match a node labeled "LoginService" without the word "authentication" appearing in it.

**Impact**: Memory retrieval misses semantically related but textually different concepts. This is the #1 quality issue for a memory agent.

**Current matching**:
```haskell
-- In SelectContext.hs
matchScore :: Node -> [Text] -> Int
matchScore node terms =
  let lower = T.toLower (nodeLabel node)
  in sum [1 | t <- terms, T.isInfixOf t lower]
```

**Needed matching**:
```haskell
-- Hybrid: substring + embedding similarity + community proximity
hybridMatchScore :: Node -> QueryEmbedding -> Graph -> Double
hybridMatchScore node queryEmb g =
  let textScore = matchScore node (queryTerms queryEmb)
      embScore  = cosineSimilarity (nodeEmbedding node) (queryVector queryEmb)
      commScore = communityProximity node queryEmb g
  in textScore + 0.7 * embScore + 0.3 * commScore
```

**Effort**: 3-5 days

**Implementation**:
- Add `nodeEmbedding :: Maybe Vector` to `Node` type
- New `Domain.Embedding` module with `EmbeddingVector` type
- New `UseCase.SemanticSearch` module
- Embedding generation: local (sentence-transformers via subprocess) or API (OpenAI embeddings)
- Vector index: simple cosine similarity store (no external DB needed for <100k nodes)
- Hybrid search: combine `matchScore` with `embeddingScore`

---

## Gap 3: LLM-Driven Summarization 🟠 High

**Problem**: `ConversationNode.convSummary` is empty when reconstructed from graph nodes. The LLM client (`Infrastructure.LLM.OpenAI`) exists but isn't wired to conversation creation.

**Impact**: Without summaries, `conversation_history` returns raw question text only. Summaries enable:
- Compact context for future queries
- Semantic search over past answers
- Token-efficient memory retrieval

**Current state**:
```haskell
-- In UseCase.Conversation
nodeToConversation n g = ConversationNode
  { convSummary = ""  -- Summary not stored in node label
  , ...
  }
```

**Needed**:
```haskell
summarizeWithLLM :: LLMConfig -> ConversationNode -> IO ConversationNode
```

**Effort**: 1-2 days

**Implementation**:
- Wire `Infrastructure.LLM.OpenAI` to conversation creation flow
- Add `PipelineConfig` field for LLM config (or reuse existing `LabelingConfig`)
- MCP `add_conversation` calls summarization before saving
- Fallback: if no LLM available, use question text as summary

---

## Gap 4: Temporal Awareness 🟡 Medium

**Problem**: No time-based relevance scoring. A question from 6 months ago about authentication gets the same weight as one from yesterday.

**Impact**: Memory agents should prioritize recent conversations and knowledge. Without temporal decay, retrieval quality degrades over time.

**Current**: `Node` has `nodeCapturedAt :: Maybe Text` but it's never used in context selection.

**Needed**:
```haskell
-- Recency boost: conversations from last 24h get 2× score
-- Conversations older than 30 days get 0.5× score
recencyBoost :: UTCTime -> NodeId -> Graph -> Double
temporalSelectContext :: UTCTime -> Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
```

**Effort**: 1 day

**Implementation**:
- Parse `nodeCapturedAt` timestamps
- Add exponential decay function in `Domain.Context`
- Modify `selectContext` and `relevanceScore` to factor in recency
- Add time-boosted variants of all 4 selection strategies

---

## Gap 5: Incremental Graph Mutation via MCP 🟡 Medium

**Problem**: The MCP server can only read the graph. There's no way to add nodes or edges at runtime (beyond conversations).

**Impact**: An agent can't learn new connections from analysis. If an LLM discovers that "AuthService depends on TokenManager", it can't record that relationship.

**Current MCP tools**: All read-only (except `add_conversation`).

**Needed MCP tools**:
```
add_node      — insert a single node
add_edge      — insert an edge between two nodes
remove_node   — remove a node and its edges
update_node   — update node metadata
```

**Effort**: 2-3 days

**Implementation**:
- Extend MCP tool dispatch
- Add mutation functions to `Domain.Graph.Core`
- Need incremental community update (don't re-run full Leiden)
- Consider: local community adjustment when adding nodes

---

## Gap 6: Cross-Session Graph Persistence 🟡 Medium

**Problem**: Conversations are saved to disk but not merged back into `graph.json`. On restart, the MCP server loads `graph.json` + `memory/` directory, but there's no mechanism to persist conversational edges back to the main graph.

**Impact**: Conversations from session A aren't visible in session B's graph structure (they ARE visible via `conversation_history` disk search, but not via `select_context` graph queries).

**Current flow**:
```
Session 1: add_conversation → saves to memory/conv_xxx.md
Session 2: loads graph.json + memory/ → enriches community map with disk conversations
           → BUT original graph.json doesn't have conversation nodes
           → select_context queries graph nodes only, misses conversations in node matching
```

**Needed flow**:
```
Session 1: add_conversation → saves to memory/ + updates in-memory graph
           → periodically: write enriched graph back to graph.json
Session 2: loads enriched graph.json → conversations already in graph nodes
           → select_context finds conversations via community-aware search
```

**Effort**: 1 day

**Implementation**:
- On `add_conversation`: update TVar + save to disk
- Add `mergeConversationsToGraph :: Graph -> [ConversationNode] -> Graph`
- Periodic write-back: either on shutdown or after N mutations
- Or: always load from disk on startup by merging `memory/` into graph

---

## Gap 7: Bidirectional Conversation Edges 🟢 Low

**Problem**: Conversation edges are one-way: conversation → code. There are no back-edges from code nodes to conversations that reference them.

**Impact**: When exploring a code node, you can't find "which conversations discussed this?" without a full graph scan.

**Current**:
```haskell
chatEdgesForConversation conv =
  [ Edge { edgeSource = convId conv, edgeTarget = codeNodeId, ... }
  | codeNodeId <- convRelevantNodes conv
  ]
```

**Needed**: Reverse index for "find conversations mentioning this node."

**Effort**: 0.5 days (just an index, not new edges)

**Implementation**:
- Build reverse index: `Map NodeId [ConversationId]`
- Add `conversationsForNode :: NodeId -> Graph -> [ConversationNode]`
- Use in `selectContext` to boost nodes with recent conversations

---

## Gap 8: Differential Context 🟢 Low

**Problem**: `SelectionStrategy` has a `DifferentialContext` constructor but no implementation. Every context selection starts from scratch, even if the previous query selected most of the same nodes.

**Impact**: Wasted tokens. If you asked about "authentication" and then ask about "token validation", you re-send most of the same context.

**Current**: `DifferentialContext` case falls through to `CommunityAware`.

**Needed**: Calculate diff between previous and current context, only send new nodes/edges.

**Effort**: 1-2 days

**Implementation**:
- Track previous `SelectedContext`
- Compute `contextDiff :: SelectedContext -> SelectedContext -> SelectedContext`
- Send only new nodes/edges + reference to previous context

---

## Priority Matrix

| Gap | Priority | Effort | Impact if Missing |
|-----|----------|--------|-------------------|
| 1. Real-time graph mutation | 🔴 Critical | 2-3 days | Agent can't see its own memories |
| 2. Semantic search | 🔴 Critical | 3-5 days | Memory retrieval misses semantic matches |
| 3. LLM summarization | 🟠 High | 1-2 days | Conversations lack summaries |
| 4. Temporal awareness | 🟡 Medium | 1 day | Old memories equal weight as new |
| 5. Incremental MCP mutation | 🟡 Medium | 2-3 days | Agent can't record new relationships |
| 6. Cross-session persistence | 🟡 Medium | 1 day | Conversations not in graph queries |
| 7. Bidirectional edges | 🟢 Low | 0.5 days | Can't find "who discussed this?" |
| 8. Differential context | 🟢 Low | 1-2 days | Redundant token usage |

**Total estimated effort: 12-18 days**

## Risk Assessment

| Risk | Mitigation |
|------|-------------|
| Embedding model size (memory) | Start with API-based embeddings, local model later |
| Community detection on every mutation | Use local community adjustment, not full Leiden |
| MCP protocol compatibility | MCP spec is stable, tools are additive |
| Graph corruption from mutations | Write-ahead log, periodic snapshots |
| LLM summarization cost | Use gpt-4o-mini or local model for summaries |