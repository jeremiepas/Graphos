# 05 — Technical Specifications

Detailed specifications for each memory agent enhancement.

---

## Spec 1: Mutable Graph State

### Interface

```haskell
module Graphos.Infrastructure.Server.MCPState
  ( MemoryAgentState(..)
  , initMemoryAgentState
  , snapshotGraph
  , snapshotIfDirty
  , addConversationToState
  , addNodeToState
  , addEdgeToState
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)

data MemoryAgentState = MemoryAgentState
  { masGraph      :: TVar Graph
  , masIndex      :: TVar GraphIndex
  , masCommMap    :: TVar CommunityMap
  , masAnalysis   :: TVar Analysis
  , masConvs      :: TVar [ConversationNode]
  , masDirty      :: TVar Bool
  , masMutationCount :: TVar Int
  }
```

### Operations

```haskell
-- | Initialize state from loaded graph + disk conversations
initMemoryAgentState :: Graph -> CommunityMap -> Analysis -> IO MemoryAgentState

-- | Add a conversation to in-memory state + disk
addConversationToState :: MemoryAgentState -> ConversationNode -> IO ()

-- | Add a node to in-memory state
addNodeToState :: MemoryAgentState -> Node -> IO ()

-- | Add an edge to in-memory state
addEdgeToState :: MemoryAgentState -> Edge -> IO ()

-- | Save graph to disk if dirty
snapshotIfDirty :: MemoryAgentState -> FilePath -> IO ()

-- | Force save graph to disk
snapshotGraph :: MemoryAgentState -> FilePath -> IO ()

-- | Rebuild index after graph changes
rebuildIndex :: MemoryAgentState -> IO ()
```

### Snapshot Policy

- After every **10 mutations** (`masMutationCount`):
  - Write `graph.json` atomically (write to temp file, then rename)
  - Reset `masDirty` flag
- On **SIGTERM / SIGINT**: force snapshot
- On **`snapshot` MCP command**: force snapshot

### Thread Safety

All state modifications use STM (`atomically`):
```haskell
addConversationToState state conv = do
  -- Save to disk (IO, outside STM)
  saveConversationToFile "graphos-out/memory" conv
  -- Update in-memory state (STM)
  atomically $ do
    modifyTVar' (masGraph state) (insertConversationNode conv)
    modifyTVar' (masCommMap state) (enrichWithChatHistory [conv])
    modifyTVar' (masConvs state) (conv:)
    modifyTVar' (masMutationCount state) (+1)
    writeTVar (masDirty state) True
```

---

## Spec 2: Embedding Types

### Domain Types

```haskell
module Graphos.Domain.Embedding
  ( EmbeddingVector(..)
  , cosineSimilarity
  , euclideanDistance
  , embeddingDimension
  , zeroEmbedding
  , EmbeddingProvider(..)
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig
  ) where

import qualified Data.Vector.Unboxed as VU

-- | A dense embedding vector for semantic similarity search.
newtype EmbeddingVector = EmbeddingVector
  { unEmbedding :: VU.Vector Double }
  deriving (Eq, Show, Generic, NFData)

-- | Cosine similarity between two embedding vectors.
-- Returns value in [-1, 1]. 1.0 = identical, 0.0 = unrelated, -1.0 = opposite.
cosineSimilarity :: EmbeddingVector -> EmbeddingVector -> Double

-- | Euclidean distance between two embedding vectors.
euclideanDistance :: EmbeddingVector -> EmbeddingVector -> Double

-- | Dimension of an embedding vector.
embeddingDimension :: EmbeddingVector -> Int

-- | Zero vector of given dimension (for nodes without embeddings).
zeroEmbedding :: Int -> EmbeddingVector

-- | Embedding provider configuration.
data EmbeddingProvider
  = OpenAIEmbeddings        -- text-embedding-3-small (1536 dims)
  | OllamaEmbeddings Text   -- Local Ollama API endpoint
  | NoEmbeddings            -- Disable embeddings
  deriving (Eq, Show, Generic)

data EmbeddingConfig = EmbeddingConfig
  { ecProvider    :: EmbeddingProvider
  , ecModel       :: Text       -- Model name (e.g., "text-embedding-3-small")
  , ecDimension   :: Int        -- Vector dimension (e.g., 1536)
  , ecApiKey      :: Maybe Text -- API key (or env var reference)
  , ecBaseUrl     :: Maybe Text -- Custom endpoint
  } deriving (Eq, Show, Generic)

defaultEmbeddingConfig :: EmbeddingConfig
defaultEmbeddingConfig = EmbeddingConfig
  { ecProvider  = NoEmbeddings
  , ecModel     = "text-embedding-3-small"
  , ecDimension = 1536
  , ecApiKey    = Nothing
  , ecBaseUrl   = Nothing
  }
```

### Node Extension

```haskell
-- In Domain.Types.Node, add field:
data Node = Node
  { ...
  , nodeEmbedding :: Maybe EmbeddingVector  -- Dense embedding for semantic search
  }
```

This field is `Maybe` for backward compatibility — nodes without embeddings still work with text search.

### JSON Format

Embeddings stored separately in `graphos-out/embeddings.json`:
```json
{
  "model": "text-embedding-3-small",
  "dimension": 1536,
  "nodes": {
    "AuthService": [0.012, -0.034, 0.056, ...],
    "TokenStore": [-0.078, 0.091, 0.023, ...]
  }
}
```

This avoids bloating the main `graph.json` with potentially megabytes of float arrays.

---

## Spec 3: Hybrid Search

### Scoring

```haskell
module Graphos.UseCase.SemanticSearch
  ( hybridSearch
  , semanticSearchNodes
  , SearchResult(..)
  , SearchMode(..)
  ) where

data SearchMode
  = TextOnly          -- Substring matching (current behavior)
  | EmbeddingOnly     -- Cosine similarity only
  | Hybrid            -- Weighted combination (default)

data SearchResult = SearchResult
  { srNodeId    :: NodeId
  , srLabel     :: Text
  , srScore     :: Double      -- Total weighted score
  , srTextScore :: Int         -- Substring match count
  , srEmbScore  :: Double      -- Cosine similarity (0 if no embeddings)
  , srCommScore :: Double      -- Community proximity bonus
  }

hybridSearch :: Graph -> CommunityMap -> Maybe EmbeddingIndex -> Text -> Int -> SearchMode -> [SearchResult]
```

### Weight Configuration

```haskell
data SearchWeights = SearchWeights
  { swTextWeight :: Double  -- Default: 1.0
  , swEmbWeight  :: Double  -- Default: 0.7
  , swCommWeight :: Double  -- Default: 0.3
  }

-- totalScore = textScore * swTextWeight 
--            + embScore * swEmbWeight 
--            + commScore * swCommWeight
```

### Embedding Index

For large graphs (10k+ nodes), brute-force cosine similarity is slow. Use a simple index:

```haskell
module Graphos.Domain.Graph.EmbeddingIndex
  ( EmbeddingIndex(..)
  , buildEmbeddingIndex
  , searchEmbeddingIndex
  ) where

data EmbeddingIndex = EmbeddingIndex
  { eiVectors  :: VU.Vector Double      -- Flat array: node_count * dimension
  , eiIds      :: V.Vector NodeId        -- Node IDs in same order
  , eiDim      :: Int                    -- Embedding dimension
  }

buildEmbeddingIndex :: [(NodeId, EmbeddingVector)] -> Int -> EmbeddingIndex
searchEmbeddingIndex :: EmbeddingVector -> Int -> EmbeddingIndex -> [(NodeId, Double)]
```

For < 10k nodes: brute-force (simple, correct).
For 10k-100k nodes: flat array with batch dot products.
For 100k+ nodes: consider HNSW (future enhancement).

---

## Spec 4: LLM Summarization

### Configuration

```yaml
# In graphos.yaml
summarization:
  provider: openai         # openai | ollama | none
  model: gpt-4o-mini      # Small, cheap model for summaries
  api_key: "${OPENAI_API_KEY}"
  base_url: "https://api.openai.com/v1"
  max_tokens: 100          # Keep summaries short
```

### Domain Types

```haskell
-- In Domain.Config
data SummarizationConfig = SummarizationConfig
  { scProvider  :: Text       -- "openai" | "ollama" | "none"
  , scModel     :: Text       -- "gpt-4o-mini" | "llama3.2"
  , scApiKey    :: Maybe Text -- API key or env var
  , scBaseUrl   :: Maybe Text -- Custom endpoint
  , scMaxTokens :: Int        -- Max tokens for summary (default: 100)
  } deriving (Eq, Show, Generic)
```

### Summarization Flow

```haskell
-- In UseCase.Conversation
summarizeConversationWithLLM :: SummarizationConfig -> ConversationNode -> IO ConversationNode
summarizeConversationWithLLM cfg conv
  | scProvider cfg == "none" = pure conv { convSummary = convQuestion conv }
  | otherwise = do
      let prompt = T.unlines
            [ "Summarize this programming question in 1-2 sentences."
            , "Focus on the technical topic, not the question format."
            , ""
            , "Question: " <> convQuestion conv
            ]
      result <- callLLM cfg prompt
      case result of
        Right summary -> pure conv { convSummary = T.strip summary }
        Left _        -> pure conv { convSummary = convQuestion conv }  -- fallback
```

### MCP Integration

`add_conversation` tool updated:

**Old behavior**: `answer_summary` is optional, defaults to "(no summary)"
**New behavior**:
1. If `answer_summary` is provided → use it directly
2. If `answer_summary` is empty and LLM configured → generate summary
3. If no LLM available → use question text as summary

---

## Spec 5: Temporal Scoring

### Configuration

```yaml
# In graphos.yaml
temporal:
  half_life_days: 7      # Decay half-life (default: 7 days)
  max_boost: 2.0          # Maximum recency boost (default: 2.0)
  min_boost: 0.1           # Minimum boost for old nodes (default: 0.1)
```

### Domain Types

```haskell
data TemporalConfig = TemporalConfig
  { tcHalfLife :: NominalDiffTime  -- Default: 7 days
  , tcMaxBoost :: Double           -- Default: 2.0
  , tcMinBoost :: Double           -- Default: 0.1
  } deriving (Eq, Show)

defaultTemporalConfig :: TemporalConfig
defaultTemporalConfig = TemporalConfig
  { tcHalfLife = 7 * 86400  -- 7 days in seconds
  , tcMaxBoost = 2.0
  , tcMinBoost = 0.1
  }
```

### Scoring Formula

```
recencyBoost(ts, now) = max(minBoost, maxBoost * exp(-λ * age_hours))

where:
  λ = ln(2) / (halfLife_hours)
  age_hours = (now - ts) / 3600
  maxBoost = 2.0 (conversations from last hour)
  minBoost = 0.1 (conversations older than 30 days)
```

Decay table (halfLife = 7 days):

| Age | Boost |
|-----|-------|
| 1 hour | 2.00 |
| 1 day | 1.91 |
| 3 days | 1.74 |
| 7 days | 1.00 |
| 14 days | 0.50 |
| 30 days | 0.17 |
| 60 days | 0.10 (minimum) |

### Integration Point

```haskell
-- In UseCase.SelectContext
relevanceScoreWithTemporal :: TemporalConfig -> UTCTime -> NodeId -> Graph -> [Text] -> Double
relevanceScoreWithTemporal cfg now nid g terms =
  let base = relevanceScore nid g terms
      temporal = recencyBoost cfg now (getNodeData nid g)
  in base * temporal
```

---

## Spec 6: Incremental Community Assignment

### Algorithm

When a new node is added to the graph:

1. Find all neighbors of the new node
2. Group neighbors by community
3. Assign to the community with the most neighbors
4. If no neighbors: assign to community 0 (or create new community)

```haskell
assignNodeToCommunity :: NodeId -> Graph -> CommunityMap -> CommunityId
assignNodeToCommunity nid g commMap =
  let nodeCommMap = buildNodeCommunityMap commMap
      nbs = Set.toList (neighbors g nid)
      neighborCommunities = [Map.lookup nb nodeCommMap | nb <- nbs]
      communityCounts = Map.fromListWith (+) [(cid, 1) | Just cid <- neighborCommunities]
  in case Map.toList communityCounts of
       []          -> 0  -- No neighbors: assign to chat community
       ((cid, _):_) -> cid  -- Assign to largest-neighbor community
```

### Re-clustering Trigger

After N mutations (configurable, default: 50), trigger a full Leiden re-cluster:
- Threshold check: `mutationCount >= reClusterThreshold`
- Re-run `detectCommunities` on the entire graph
- Update `CommunityMap` and `Analysis`

---

## Spec 7: MCP Tool Additions

### add_node

```json
{
  "name": "add_node",
  "description": "Add a node to the knowledge graph. The node will be assigned to a community based on its connections.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "label": { "type": "string", "description": "Human-readable label" },
      "kind": { "type": "string", "description": "Node kind: Function, Class, Module, Concept, etc." },
      "source_file": { "type": "string", "description": "Source file path" },
      "signature": { "type": "string", "description": "Type signature or declaration" }
    },
    "required": ["label"]
  }
}
```

**Response**:
```json
{
  "node_id": "concept_Authentication",
  "label": "Authentication",
  "community": 3,
  "status": "added"
}
```

### add_edge

```json
{
  "name": "add_edge",
  "description": "Add an edge between two nodes in the knowledge graph.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "source": { "type": "string", "description": "Source node ID" },
      "target": { "type": "string", "description": "Target node ID" },
      "relation": { "type": "string", "description": "Relation: calls, references, contains, depends_on, etc." },
      "confidence": { "type": "string", "enum": ["EXTRACTED", "INFERRED", "AMBIGUOUS"], "default": "INFERRED" }
    },
    "required": ["source", "target", "relation"]
  }
}
```

**Response**:
```json
{
  "edge_id": "AuthService_calls_TokenStore",
  "source": "AuthService",
  "target": "TokenStore",
  "relation": "calls",
  "confidence": "INFERRED",
  "status": "added"
}
```

### search_semantic

```json
{
  "name": "search_semantic",
  "description": "Search nodes using hybrid text + semantic similarity. Returns ranked results with scores.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": { "type": "string", "description": "Search query" },
      "mode": { "type": "string", "enum": ["hybrid", "text", "embedding"], "default": "hybrid" },
      "limit": { "type": "integer", "default": 10, "description": "Maximum results" }
    },
    "required": ["query"]
  }
}
```

**Response**:
```json
{
  "results": [
    {
      "node_id": "AuthService",
      "label": "AuthService",
      "score": 0.92,
      "text_score": 1,
      "embedding_score": 0.87,
      "community_score": 0.3,
      "community_id": 3
    }
  ],
  "mode": "hybrid",
  "total": 1
}
```

---

## Spec 8: Configuration Changes

### graphos.yaml additions

```yaml
# ──── Embedding Configuration ──────────────────
# Used by: graphos . --embed or MCP search_semantic
embedding:
  # Provider: openai, ollama, or none (substring matching only)
  provider: none
  # Model name (OpenAI: text-embedding-3-small, Ollama: nomic-embed-text)
  model: "text-embedding-3-small"
  # Vector dimension (must match model output)
  dimension: 1536
  # API key (use ${ENV_VAR} for environment variables)
  api_key: "${OPENAI_API_KEY}"
  # Base URL (default: OpenAI, or Ollama endpoint)
  base_url: "https://api.openai.com/v1"

# ──── Summarization Configuration ──────────────
# Used by: add_conversation MCP tool (auto-summarize if no summary provided)
summarization:
  # Provider: openai, ollama, or none (use question as summary)
  provider: none
  # Model for summaries (small, cheap model recommended)
  model: "gpt-4o-mini"
  # API key
  api_key: "${OPENAI_API_KEY}"
  # Base URL
  base_url: "https://api.openai.com/v1"
  # Maximum tokens for summary
  max_tokens: 100

# ──── Temporal Configuration ─────────────────────
# Used by: select_context (recency boosting)
temporal:
  # Decay half-life in days (default: 7)
  half_life_days: 7
  # Maximum recency boost for recent nodes (default: 2.0)
  max_boost: 2.0
  # Minimum boost for old nodes (default: 0.1)
  min_boost: 0.1

# ──── Memory Agent Configuration ────────────────
# Used by: MCP server
memory_agent:
  # Enable real-time graph mutation (default: true)
  mutable: true
  # Number of mutations before periodic snapshot (default: 10)
  snapshot_interval: 10
  # Re-clustering threshold: number of mutations before full Leiden re-run (default: 50)
  recluster_threshold: 50
```

### PipelineConfig additions

```haskell
-- In Domain.Types.Pipeline
data PipelineConfig = PipelineConfig
  { ...
  , cfgEmbeddingConfig    :: EmbeddingConfig      -- NEW
  , cfgSummarizationConfig :: SummarizationConfig -- NEW
  , cfgTemporalConfig      :: TemporalConfig       -- NEW
  , cfgMutableAgent        :: Bool                 -- NEW: Enable mutable MCP state
  , cfgSnapshotInterval    :: Int                  -- NEW: Mutations before snapshot
  , cfgReclusterThreshold  :: Int                  -- NEW: Mutations before re-cluster
  }
```

---

## Spec 9: Testing Strategy

### Unit Tests

| Module | Test | Priority |
|--------|------|----------|
| `Domain.Embedding` | `cosineSimilarity` correctness | Phase 2 |
| `Domain.Embedding` | `euclideanDistance` correctness | Phase 2 |
| `UseCase.SemanticSearch` | Hybrid scoring formula | Phase 2 |
| `UseCase.Temporal` | `recencyBoost` decay curve | Phase 3 |
| `UseCase.Temporal` | Edge cases: no timestamp, future timestamp | Phase 3 |
| `UseCase.Conversation` | `summarizeConversationWithLLM` fallback | Phase 3 |
| `Domain.Graph.Core` | `insertNode`, `insertEdge`, `removeNode` | Phase 4 |
| `Domain.Community` | `assignNodeToCommunity` | Phase 4 |

### Integration Tests

| Test | What | Priority |
|------|------|----------|
| MCP mutable state | Add conversation → immediately visible in `select_context` | Phase 1 |
| Cross-session persistence | Save conv → restart → conv in graph | Phase 1 |
| Semantic search | "authentication" matches "LoginService" with embeddings | Phase 2 |
| LLM summarization | Add conversation → summary generated | Phase 3 |
| Temporal scoring | Recent conversation scores higher than old | Phase 3 |
| Incremental mutation | Add node → visible in `get_node` | Phase 4 |
| Community assignment | New node assigned to neighbor's community | Phase 4 |

### Property-Based Tests

```haskell
-- Cosine similarity is symmetric
prop_cosineSimilaritySymmetric :: EmbeddingVector -> EmbeddingVector -> Bool
prop_cosineSimilaritySymmetric a b = 
  abs (cosineSimilarity a b - cosineSimilarity b a) < 1e-10

-- Identical vectors have similarity 1.0
prop_cosineSimilarityIdentical :: EmbeddingVector -> Bool
prop_cosineSimilarityIdentical v = 
  abs (cosineSimilarity v v - 1.0) < 1e-10

-- recencyBoost decreases over time
prop_recencyBoostMonotonic :: TemporalConfig -> Node -> Bool
prop_recencyBoostMonotonic cfg node =
  recencyBoost cfg t1 node >= recencyBoost cfg t2 node  -- when t1 < t2

-- insertNode then get node returns same node
prop_insertNodeGetNode :: Graph -> Node -> Bool
prop_insertNodeGetNode g n = 
  Map.lookup (nodeId n) (gNodes (insertNode n g)) == Just n
```