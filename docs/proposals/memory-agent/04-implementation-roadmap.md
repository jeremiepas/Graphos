# 04 — Implementation Roadmap

Phased implementation plan for Graphos memory agent capabilities.

## Phase Overview

| Phase | Focus | Duration | Deliverable |
|-------|-------|----------|-------------|
| Phase 1 | Mutable Graph + Persistence | 3-4 days | Real-time memory within sessions |
| Phase 2 | Semantic Search | 4-5 days | Embedding-based retrieval |
| Phase 3 | LLM Summarization + Temporal | 2-3 days | Smart summaries + time-awareness |
| Phase 4 | Incremental Mutation + MCP Tools | 3-4 days | Full graph mutation via MCP |
| Phase 5 | Polish + Optimization | 1-2 days | Differential context, optimization |

**Total: 13-18 days**

---

## Phase 1: Mutable Graph + Cross-Session Persistence

**Goal**: Conversations added via MCP are immediately visible in the same session and persist across restarts.

**Duration**: 3-4 days

### Day 1-2: Mutable MCP State

**Files to modify**:
- `src/Graphos/Infrastructure/Server/MCP.hs` — Replace immutable `Graph` with `MemoryAgentState`
- `src/Graphos/Domain/Context.hs` — No changes (types already complete)

**Changes**:

1. Create `MemoryAgentState`:
```haskell
data MemoryAgentState = MemoryAgentState
  { masGraph    :: TVar Graph
  , masIndex    :: TVar GraphIndex
  , masCommMap  :: TVar CommunityMap
  , masAnalysis :: TVar Analysis
  , masConvs    :: TVar [ConversationNode]
  , masDirty    :: TVar Bool
  }
```

2. Initialize from loaded graph + disk conversations:
```haskell
initMemoryAgentState :: Graph -> CommunityMap -> Analysis -> IO MemoryAgentState
```

3. Update all MCP handlers to read from `TVar`:
```haskell
-- Before:
handleRequest :: Graph -> CommunityMap -> Analysis -> MCPRequest -> IO ()

-- After:
handleRequest :: MemoryAgentState -> MCPRequest -> IO ()
-- Each handler reads current state from TVars
```

4. Update `add_conversation` to modify TVars:
```haskell
handleAddConversation :: MemoryAgentState -> KM.KeyMap Value -> IO (Either Text Value)
handleAddConversation state args = do
  -- ... create ConversationNode ...
  -- Save to disk
  saveConversationToFile "graphos-out/memory" conv
  -- Update in-memory state
  atomically $ do
    modifyTVar' (masGraph state) (insertNode (conversationNodeToNode conv))
    modifyTVar' (masCommMap state) (enrichWithChatHistory [conv])
    modifyTVar' (masConvs state) (conv:)
    writeTVar (masDirty state) True
```

### Day 3: Cross-Session Persistence

**Files to modify**:
- `src/Graphos/Infrastructure/Server/MCP.hs` — Add periodic snapshot
- `src/Graphos/Infrastructure/FileSystem/Conversation.hs` — Add merge function
- `src/Graphos/UseCase/Export.hs` — Add graph snapshot function

**Changes**:

1. Add `mergeConversationsToGraph`:
```haskell
mergeConversationsToGraph :: Graph -> CommunityMap -> [ConversationNode] -> (Graph, CommunityMap)
```

2. On startup, merge `memory/` conversations into graph:
```haskell
startMCPServerFromFile path = do
  loaded <- loadGraphFromFile path
  diskConvs <- loadConversationsFromDir "graphos-out/memory"
  let g = lrGraph loaded `mergeConversations` diskConvs
      commMap = lrCommunities loaded `enrichWithChatHistory` diskConvs
  state <- initMemoryAgentState g commMap (lrAnalysis loaded)
  startMCPServer state
```

3. Periodic snapshot (every 10 mutations):
```haskell
snapshotIfDirty :: MemoryAgentState -> FilePath -> IO ()
snapshotIfDirty state path = do
  dirty <- readTVarIO (masDirty state)
  when dirty $ do
    g <- readTVarIO (masGraph state)
    ExportJSON.saveGraph g path
    atomically $ writeTVar (masDirty state) False
```

4. On shutdown (SIGTERM handler): final snapshot.

### Day 3-4: Testing + Integration

**Tests to add**:
- `Graphos.Infrastructure.Server.MCPSpec` — Test mutable state
- `Graphos.UseCase.ConversationSpec` — Test in-memory conversation insertion
- `Graphos.Infrastructure.FileSystem.ConversationSpec` — Test merge function

**Acceptance criteria**:
- [ ] `add_conversation` creates node visible in `select_context` immediately
- [ ] `conversation_history` returns conversations added in current session
- [ ] `select_context` with `include_history=true` includes new conversations
- [ ] On restart, previously saved conversations are loaded and queryable
- [ ] Graph snapshot written after every 10 mutations

---

## Phase 2: Semantic Search

**Goal**: Hybrid text + embedding search for node and conversation retrieval.

**Duration**: 4-5 days

### Day 1-2: Embedding Types + Generation

**New files**:
- `src/Graphos/Domain/Embedding.hs` — Pure embedding types
- `src/Graphos/Infrastructure/Embedding/OpenAI.hs` — OpenAI embeddings API
- `src/Graphos/Infrastructure/Embedding/Local.hs` — Local model (sentence-transformers)

**Domain types**:
```haskell
module Graphos.Domain.Embedding
  ( EmbeddingVector(..)
  , cosineSimilarity
  , euclideanDistance
  , EmbeddingProvider(..)
  , EmbeddingConfig(..)
  ) where

newtype EmbeddingVector = EmbeddingVector
  { unEmbedding :: Vector Double }
  deriving (Eq, Show, Generic, NFData)

cosineSimilarity :: EmbeddingVector -> EmbeddingVector -> Double
euclideanDistance :: EmbeddingVector -> EmbeddingVector -> Double

data EmbeddingProvider
  = OpenAIEmbeddings
  | LocalModel FilePath
  | OllamaEmbeddings Text
  | NoEmbeddings
  deriving (Eq, Show, Generic)

data EmbeddingConfig = EmbeddingConfig
  { ecProvider  :: EmbeddingProvider
  , ecModel     :: Text
  , ecDimension :: Int
  , ecApiKey    :: Maybe Text
  , ecBaseUrl   :: Maybe Text
  }
```

**Extend `Node` type**:
```haskell
-- In Domain.Types.Node, add:
, nodeEmbedding :: Maybe EmbeddingVector
```

**Embedding generation**:
```haskell
-- Infrastructure layer
generateEmbedding :: EmbeddingConfig -> Text -> IO (Either Text EmbeddingVector)
generateEmbeddings :: EmbeddingConfig -> [Text] -> IO (Either Text [EmbeddingVector])
```

### Day 3: Vector Index + Hybrid Search

**New file**: `src/Graphos/UseCase/SemanticSearch.hs`

```haskell
module Graphos.UseCase.SemanticSearch
  ( hybridSearch
  , semanticSearchNodes
  , SemanticResult(..)
  , SearchResult(..)
  ) where

data SearchResult = SearchResult
  { srNodeId   :: NodeId
  , srScore    :: Double
  , srTextMatch :: Int      -- substring match count
  , srEmbScore  :: Double  -- cosine similarity (0 if no embeddings)
  , srCommScore :: Double  -- community proximity bonus
  }

hybridSearch :: Graph -> CommunityMap -> Maybe EmbeddingIndex -> Text -> Int -> [SearchResult]
```

**Scoring formula**:
```
totalScore = textScore * 1.0 + embScore * 0.7 + commScore * 0.3
```

Where:
- `textScore` = current `matchScore` (substring matching)
- `embScore` = cosine similarity of node embedding to query embedding
- `commScore` = community proximity bonus (nodes in same community as top matches)

### Day 4-5: Integration + Testing

**Modified files**:
- `src/Graphos/UseCase/SelectContext.hs` — Use `hybridSearch` instead of `matchScore`
- `src/Graphos/Infrastructure/Server/MCP.hs` — Add `search_semantic` tool
- `src/Graphos/Domain/Config.hs` — Add `EmbeddingConfig` to `GraphosConfig`

**New MCP tool**:
```json
{
  "name": "search_semantic",
  "description": "Search nodes using hybrid text + semantic similarity",
  "inputSchema": {
    "query": "Search query",
    "mode": "hybrid|text|embedding",
    "limit": 10
  }
}
```

**Acceptance criteria**:
- [ ] `search_semantic` returns semantically similar nodes
- [ ] Hybrid mode outperforms pure substring on conceptual queries
- [ ] Fallback to text-only when no embeddings available
- [ ] OpenAI embeddings API integration works
- [ ] Performance: < 100ms for 10k nodes

---

## Phase 3: LLM Summarization + Temporal Awareness

**Goal**: Auto-summarize conversations; boost recent results.

**Duration**: 2-3 days

### Day 1: LLM Summarization

**Modified files**:
- `src/Graphos/Infrastructure/LLM/OpenAI.hs` — Add `summarize` function
- `src/Graphos/UseCase/Conversation.hs` — Add `summarizeConversationWithLLM`
- `src/Graphos/Infrastructure/Server/MCP.hs` — Wire summarization to `add_conversation`

**Flow**:
```haskell
summarizeConversationWithLLM :: LLMConfig -> ConversationNode -> IO ConversationNode
summarizeConversationWithLLM llmCfg conv = do
  let prompt = "Summarize in 1-2 sentences: " <> convQuestion conv
  result <- callLLM llmCfg prompt
  case result of
    Right summary -> pure conv { convSummary = summary }
    Left _        -> pure conv { convSummary = convQuestion conv }  -- fallback
```

**MCP update**: `add_conversation` now optionally accepts `answer_summary` — if empty, generate via LLM.

### Day 2: Temporal Scoring

**New file**: `src/Graphos/UseCase/Temporal.hs`

```haskell
module Graphos.UseCase.Temporal
  ( recencyBoost
  , temporalBoost
  , TemporalConfig(..)
  ) where

data TemporalConfig = TemporalConfig
  { tcHalfLife    :: NominalDiffTime  -- Default: 7 days
  , tcMaxBoost    :: Double           -- Default: 2.0
  , tcMinBoost    :: Double           -- Default: 0.1
  }

recencyBoost :: TemporalConfig -> UTCTime -> Node -> Double
recencyBoost cfg now node =
  case nodeCapturedAt node >>= parseTimestamp of
    Just ts  -> max (tcMinBoost cfg) (tcMaxBoost cfg * exp (-lambda * diff now ts))
    Nothing  -> tcMinBoost cfg
  where lambda = ln(2) / tcHalfLife cfg
```

**Integration**: Add `temporalBoost` to `relevanceScore` in `SelectContext`:
```haskell
relevanceScore nid g terms now =
  let base = nodeLabelScore + degBoost + edgeBoost
      temporal = recencyBoost defaultTemporalConfig now (getNodeData nid g)
  in base * temporal
```

### Day 2-3: Testing

**Acceptance criteria**:
- [ ] `add_conversation` with empty summary triggers LLM summarization
- [ ] Fallback: without LLM, summary = question text
- [ ] Recent nodes get higher scores in `select_context`
- [ ] Nodes without timestamps get minimum boost (0.1)
- [ ] Configurable half-life parameter

---

## Phase 4: Incremental Graph Mutation + New MCP Tools

**Goal**: Allow MCP agents to add nodes, add edges, and update the graph.

**Duration**: 3-4 days

### Day 1-2: Graph Mutation Primitives

**Modified files**:
- `src/Graphos/Domain/Graph/Core.hs` — Add mutation functions
- `src/Graphos/Domain/Community.hs` — Add `insertNodeIntoCommunity`

**New functions**:
```haskell
-- Domain.Graph.Core
insertNode :: Node -> Graph -> Graph
insertEdge :: Edge -> Graph -> Graph
removeNode :: NodeId -> Graph -> Graph
updateNode :: NodeId -> (Node -> Node) -> Graph -> Graph

-- Domain.Community
insertNodeIntoCommunity :: NodeId -> Graph -> CommunityMap -> CommunityId
-- Assigns node to community of most-connected neighbor
```

### Day 2-3: MCP Tool Handlers

**Modified file**: `src/Graphos/Infrastructure/Server/MCP.hs`

**New tools**:

```json
[
  {
    "name": "add_node",
    "description": "Add a node to the knowledge graph",
    "inputSchema": {
      "label": "Human-readable label",
      "kind": "Function, Class, Module, etc.",
      "source_file": "File path",
      "signature": "Type signature"
    }
  },
  {
    "name": "add_edge",
    "description": "Add an edge between two nodes",
    "inputSchema": {
      "source": "Source node ID",
      "target": "Target node ID",
      "relation": "calls, references, contains, etc.",
      "confidence": "EXTRACTED, INFERRED, or AMBIGUOUS"
    }
  }
]
```

### Day 3-4: Incremental Community Update

**Modified file**: `src/Graphos/Domain/Community.hs`

When a new node is added:
1. Find its most-connected neighbor
2. Assign it to that neighbor's community
3. Update the community map
4. Mark the graph as dirty for periodic re-clustering

**Acceptance criteria**:
- [ ] `add_node` creates node visible in `get_node` immediately
- [ ] `add_edge` creates edge between existing nodes
- [ ] New nodes are assigned to the community of their most-connected neighbor
- [ ] Periodic re-clustering after N mutations
- [ ] Error handling for missing node IDs, duplicate edges

---

## Phase 5: Polish + Optimization

**Goal**: Differential context, performance tuning, documentation.

**Duration**: 1-2 days

### Day 1: Differential Context

**Modify**: `src/Graphos/UseCase/SelectContext.hs`

Implement `DifferentialContext` strategy:
```haskell
selectDifferential :: PreviousContext -> Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectDifferential prev g commMap analysis query budget =
  let fullContext = selectContext g commMap analysis query budget
      newNodes = filter (not . inPrevious) (scNodes fullContext)
      newEdges = filter (not . inPrevious) (scEdges fullContext)
  in fullContext { scNodes = newNodes, scEdges = newEdges }
```

### Day 2: Performance + Documentation

- Profile `select_context` with 100k node graphs
- Optimize embedding search (consider HNSW for >50k nodes)
- Update README with memory agent section
- Add examples for `add_conversation`, `search_semantic`
- Write end-to-end test: full memory agent workflow

**Acceptance criteria**:
- [ ] `select_context` with `strategy=differential_context` sends only new info
- [ ] Performance: < 50ms for select_context on 10k node graph
- [ ] README updated with memory agent usage
- [ ] End-to-end test: build graph → add conversation → retrieve → verify

---

## Dependency Graph

```
Phase 1 (Mutable Graph)
  │
  ├──► Phase 2 (Semantic Search)
  │      │
  │      └──► Phase 3 (LLM + Temporal)
  │             │
  │             └──► Phase 4 (Mutation Tools)
  │                    │
  │                    └──► Phase 5 (Polish)
  │
  └──► Phase 6 (Differential Context) ← independent of Phase 2
```

Phase 1 is the critical path — everything else depends on mutable state.

## Success Metrics

| Metric | Current | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Phase 5 |
|--------|---------|---------|---------|---------|---------|---------|
| Memory persistence | Disk only | In-memory + disk | Same | Same | Same | Same |
| Retrieval quality | Substring | Substring | Hybrid | +Temporal | Same | Same |
| Real-time memory | None | Immediate | Same | Same | Same | Same |
| Graph mutation | Conversations only | Same | Same | Same | Full | Same |
| Context efficiency | Full each time | Same | Same | Same | Same | Differential |