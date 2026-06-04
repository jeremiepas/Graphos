# 01 — Current Capabilities

What Graphos already provides for building a memory agent.

## 1. Knowledge Graph Core ✅

**Status**: Production-quality, comprehensive

### Nodes

Every node in the graph carries rich metadata:

```haskell
data Node = Node
  { nodeId           :: NodeId          -- Unique identifier (file + entity name)
  , nodeLabel        :: Text            -- Human-readable label
  , nodeFileType     :: FileType        -- CodeFile | DocumentFile | PaperFile | ImageFile | VideoFile
  , nodeSourceFile   :: Text            -- Source file path
  , nodeSourceLocation :: Maybe Text    -- Line range start
  , nodeLineEnd      :: Maybe Int       -- Line range end
  , nodeKind         :: Maybe Text      -- "Function", "Class", "Method", "Interface", etc.
  , nodeSignature    :: Maybe Text      -- Type signature or declaration header
  , nodeSourceUrl    :: Maybe Text      -- Optional URL
  , nodeCapturedAt   :: Maybe Text      -- Timestamp
  , nodeAuthor       :: Maybe Text      -- Author
  , nodeContributor  :: Maybe Text      -- Contributor
  }
```

All fields strict (`StrictData`) to prevent thunk accumulation on large graphs (100k+ nodes).

### Edges

14 typed relation types with confidence scoring:

```haskell
data Relation
  = Calls | Implements | References | Cites
  | ConceptuallyRelatedTo | SharesDataWith | SemanticallySimilarTo
  | RationaleFor | Imports | ImportsFrom | Contains
  | Method | Extends | Overrides | DependsOn

data Confidence = Extracted | Inferred | Ambiguous
```

### Extraction Sources

| Type | Extensions | Method |
|------|-----------|--------|
| Code | 30+ extensions | LSP (semantic) + tree-sitter (AST fallback) |
| Docs | .md, .txt, .rst, .adoc, .org | LLM concept extraction |
| Papers | .pdf | Citation mining + concept extraction |
| Images | .png, .jpg, .webp, .gif | LLM vision |
| Video/Audio | .mp4, .mp3, .wav, etc. | Whisper transcription + LLM |
| Office | .docx, .xlsx | Markdown conversion + LLM |

## 2. Community Detection ✅

**Status**: Production-quality, Leiden algorithm

- Configurable resolution (gamma parameter: higher = fewer larger communities)
- Minimum community size enforcement
- Maximum iterations control
- Merge strategy for tiny communities
- Cohesion scoring per community

```haskell
data Resolution = Resolution
  { resGamma        :: Double   -- Resolution parameter (default: 1.0)
  , resMinSize      :: Int      -- Minimum community size (default: 3)
  , resMergeInto    :: MergeStrategy  -- How to handle tiny communities
  , resMaxIterations :: Int    -- Max Leiden iterations (default: 50)
  }
```

## 3. Context Selection Engine ✅

**Status**: Working, 4 strategies implemented

`UseCase.SelectContext` provides intelligent subgraph selection for LLM consumption:

| Strategy | Trigger | Method |
|----------|---------|--------|
| Community-aware | Focused/Module queries | Find best node → include its community + bridges |
| Relevance-weighted BFS | Exploratory queries | Score nodes by relevance, BFS within budget |
| Path-based | Cross-module queries | Shortest path between 2 concepts + neighbors |
| Architectural | "how does X work" | God nodes + bridges + community representatives |

Budget-aware: different token allocations based on query complexity.

```haskell
data QueryComplexity = Focused | ModuleLevel | CrossModule | Architectural | Exploratory

budgetForComplexity :: QueryComplexity -> Int -> ContextBudget
-- Focused:       10 nodes,  20 edges, 0.10 graph ratio
-- ModuleLevel:   30 nodes,  60 edges, 0.20 graph ratio
-- CrossModule:   50 nodes, 100 edges, 0.30 graph ratio
-- Architectural: 40 nodes,  80 edges, 0.40 graph ratio
-- Exploratory:   40 nodes,  80 edges, 0.25 graph ratio
```

## 4. Context Formatting ✅

**Status**: Working, compact output

`UseCase.FormatContext` produces markdown at ~50 tokens/node, ~20 tokens/edge:

```markdown
## Relevant Code Graph (community-aware, 30 nodes, 60 edges, 2 communities)
### Key Nodes
- AuthService [Class] [code] — src/auth/service.ts:10-85 | login(req: Request): Promise<Token>
### Key Edges
- AuthService → TokenStore [shares_data_with, INFERRED]
### Bridge Nodes
- AuthMiddleware (connects communities)
### Hub Nodes
- Router [degree=15]
### Suggested Context Expansion
- If reasoning about Authentication: include community 3 (12 nodes)
```

## 5. Conversation Memory Types ✅

**Status**: Domain types complete, persistence working

```haskell
data ConversationNode = ConversationNode
  { convId            :: Text          -- Unique exchange ID
  , convQuestion      :: Text          -- User's question
  , convSummary       :: Text          -- LLM's response summary
  , convTimestamp     :: Text          -- ISO 8601
  , convRelevantNodes :: [NodeId]      -- Code nodes referenced
  , convTokensUsed    :: Int           -- Token cost
  }

data ConversationRelation
  = Discusses         -- Conversation discusses this code node
  | RelatesTo        -- Conversation related to this code node
  | FollowUpFrom     -- Follow-up to another conversation
```

The chat community (ID 0) is reserved and added AFTER Leiden detection to prevent pollution.

```haskell
chatCommunityId :: CommunityId
chatCommunityId = 0  -- Reserved for conversation nodes

enrichWithChatHistory :: CommunityMap -> [ConversationNode] -> CommunityMap
chatEdgesForConversation :: ConversationNode -> [Edge]
conversationNodeToNode :: ConversationNode -> Node
```

## 6. Conversation Persistence ✅

**Status**: Working, YAML-frontmatter markdown files

`Infrastructure.FileSystem.Conversation` provides:

- `saveConversationToFile` — Persists to `graphos-out/memory/` as markdown with YAML frontmatter
- `loadConversationsFromDir` — Loads all conversations from directory

File format:
```markdown
---
id: "conv_2026_04_17_001"
question: "How does MCP server work?"
summary: "The MCP server exposes tools via JSON-RPC..."
timestamp: "2026-04-17T18:00:00Z"
relevant_nodes: ["AuthService", "TokenStore"]
tokens_used: 1500
---
# Q: How does MCP server work?

A: The MCP server exposes tools via JSON-RPC over stdio...
```

## 7. Conversation Retrieval ✅

**Status**: Working, substring matching

`UseCase.Conversation` provides:

- `queryConversations` — Search all document-type nodes with `memory/` prefix
- `queryConversationsFromCommunity` — Search within community 0 (faster)
- `matchConversationScore` — Score question matches 3×, summary matches 1×
- `summarizeConversation` — Compact format: "Q: {question}\nA: {summary} (N relevant nodes)"

## 8. MCP Server ✅

**Status**: Working, 11 tools exposed

The MCP server (`Infrastructure.Server.MCP`) implements JSON-RPC over stdio with these tools:

| Tool | Purpose | Memory Agent Role |
|------|---------|-------------------|
| `select_context` | Select relevant subgraph for LLM | **Core:** Context optimization |
| `add_conversation` | Store Q&A exchange | **Core:** Cross-session memory |
| `conversation_history` | Search past conversations | **Core:** Memory retrieval |
| `query_graph` | BFS/DFS traversal | Navigate the knowledge graph |
| `get_node` | Look up node by ID | Inspect specific entities |
| `get_neighbors` | Get all neighbors | Explore connections |
| `get_community` | Find community for a node | Understand module boundaries |
| `god_nodes` | High-degree hub nodes | Find architectural anchors |
| `shortest_path` | Path between 2 concepts | Trace dependencies |
| `bridge_nodes` | Articulation points | Find critical connectors |
| `graph_stats` | Node/edge counts, avg degree | Graph overview |

## 9. Graph Operations ✅

**Status**: Comprehensive

```haskell
-- Core operations (Domain.Graph.Core)
gNodes :: Graph -> Map NodeId Node
gEdges :: Graph -> Map (NodeId, NodeId) Edge
neighbors :: Graph -> NodeId -> Set NodeId
degree :: Graph -> NodeId -> Int
shortestPath :: Graph -> NodeId -> NodeId -> Maybe [NodeId]

-- Advanced operations (Domain.Graph.Query/Analysis)
articulationPoints :: Graph -> [NodeId]
biconnectedComponents :: Graph -> [[NodeId]]
dominators :: Graph -> NodeId -> Map NodeId (Maybe NodeId)

-- Community operations (Domain.Community)
detectCommunities :: Graph -> CommunityMap
scoreAllCohesion :: Graph -> CommunityMap -> CohesionMap

-- Index operations (Domain.Graph.Index)
buildIndex :: Graph -> CommunityMap -> GraphIndex
findMatchingNodes :: [Text] -> GraphIndex -> [(NodeId, Int)]
bfsFromSet :: GraphIndex -> Set NodeId -> Int -> Set NodeId
```

## 10. Export Formats ✅

| Format | Use Case |
|--------|----------|
| JSON | Persistent graph, query, MCP server input |
| HTML (vis.js) | Interactive exploration |
| Neo4j Cypher | Graph database push |
| Memgraph Cypher | In-memory graph database |
| GraphML | Gephi/yEd analysis |
| SVG | Static visualization |
| Obsidian | Personal knowledge management |
| Community Graph JSON | LLM navigation |
| Incremental JSON | Streaming write for large graphs |
| Report (markdown) | Human-readable analysis |

## 11. Pipeline ✅

```
detect() → extract() → build() → cluster() → infer() → analyze() → export()
```

With features:
- **Incremental mode** (`--update`): Only re-extracts changed files
- **Watch mode** (`--watch`): File watcher triggers incremental pipeline
- **Checkpoint/resume**: Saves pipeline state, resumes on failure
- **Streaming Neo4j**: Push nodes during extraction, edge repair pass after
- **Observability**: OpenTelemetry traces, Prometheus metrics, debug traces

## 12. Analysis ✅

```haskell
data Analysis = Analysis
  { analysisGodNodes            :: [GodNode]              -- Top-degree hubs
  , analysisSurprisingConnections :: [SurprisingConnection] -- Inter-community edges
  , analysisSuggestedQuestions  :: [SuggestedQuestion]    -- Questions to explore
  }

data GodNode = GodNode
  { gnId      :: NodeId
  , gnLabel   :: Text
  , gnEdges   :: Int
  , gnCommunities :: [CommunityId]
  }

data SurprisingConnection = SurprisingConnection
  { scEdge      :: Edge
  , scReason    :: Text  -- Why this connection is surprising
  , scDistance   :: Int   -- Community distance between endpoints
  }
```

## Summary

| Capability | Status | Completeness |
|-----------|--------|-------------|
| Knowledge graph construction | ✅ Complete | 100% |
| Community detection (Leiden) | ✅ Complete | 100% |
| Context selection (4 strategies) | ✅ Complete | 100% |
| Context formatting for LLMs | ✅ Complete | 100% |
| Conversation memory types | ✅ Complete | 100% |
| Conversation persistence | ✅ Working | 90% (substring match only) |
| Conversation retrieval | ⚠️ Partial | 70% (no embeddings) |
| MCP server interface | ✅ Working | 85% (no mutation) |
| Graph operations | ✅ Complete | 100% |
| Export formats | ✅ Complete | 100% |
| Pipeline (incremental, watch) | ✅ Complete | 100% |
| Analysis | ✅ Complete | 100% |

**Overall: ~70% of a memory agent is already built.**