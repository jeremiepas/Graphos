# Task Context: Graphos as LLM Context Optimizer

Session ID: 2026-04-17-context-optimizer
Created: 2026-04-17T18:00:00Z
Status: in_progress

## Current Request

Implement a **two-tier LLM context optimization pipeline** using Graphos's knowledge graph as a semantic filter:
- **Small local LLM** (fast, no thinking) → selects relevant subgraph → builds minimal context
- **Big LLM** (slow, thinking mode) → receives only what matters → deep analysis
- The graph acts as a **context compressor**: instead of raw files, send compact node/edge/community representations
- Conversation exchanges become graph nodes for persistent cross-session memory

Full brainstorm: `docs/brainstorm-context-optimization.md`

## Context Files (Standards to Follow)
- .opencode/context/core/standards/code-quality.md
- .opencode/context/core/standards/test-coverage.md
- .opencode/context/project-intelligence/technical-domain.md

## Reference Files (Source Material)

### Existing code to build on
- `src/Graphos/UseCase/Query.hs` — Graph querying (BFS, DFS, shortest path, matchScore, findBestNode)
- `src/Graphos/UseCase/Query/Normalize.hs` — Query normalization for search
- `src/Graphos/Domain/Types.hs` — Core types: Node, Edge, Confidence, CommunityMap, GodNode, Analysis, FileType, Relation
- `src/Graphos/Domain/Graph.hs` — Graph operations (pure)
- `src/Graphos/Domain/Community.hs` — Leiden community detection
- `src/Graphos/Domain/Analysis.hs` — Analysis (god nodes, surprises, questions)
- `src/Graphos/Infrastructure/Server/MCP.hs` — MCP server exposing graph tools (query_graph, get_node, get_neighbors, get_community, god_nodes, graph_stats, shortest_path, bridge_nodes)
- `src/Graphos/Infrastructure/Export/JSON.hs` — JSON export
- `docs/brainstorm-context-optimization.md` — Full brainstorm document with architecture, strategies, phases

### Key domain types already available
```haskell
type NodeId = Text
data Node = Node { nodeId, nodeLabel :: Text, nodeFileType :: FileType, nodeSourceFile :: Text, ... }
data Confidence = Extracted | Inferred | Ambiguous
data Relation = Calls | Imports | References | DependsOn | ...
type CommunityMap = Map CommunityId [NodeId]
data GodNode = GodNode { gnId, gnLabel :: Text, gnEdges :: Int }
data Analysis = Analysis { analysisCommunities, analysisCohesion, analysisGodNodes, analysisSurprises, analysisQuestions }
```

### Key functions already available
```haskell
queryGraph :: Graph -> Text -> Text -> Int -> QueryResult
findBestNode :: Graph -> Text -> Maybe NodeId
matchScore :: Text -> [Text] -> Int
confidenceScore :: Confidence -> Double  -- Extracted=1.0, Inferred=0.7, Ambiguous=0.2
```

## Components

### 1. Context Selection (Domain + UseCase)
- **Domain types**: ContextBudget, QueryComplexity(Focused|Module|CrossModule|Architecture|Exploratory), SelectedContext
- **Context selector**: Pure function that takes a Graph + query + budget → returns SelectedContext (compact LLM-optimized representation)
- **Scoring**: Relevance scoring using matchScore + confidenceScore + community membership + bridge/god node status
- **4 strategies**: Community-aware, Relevance-weighted BFS, Path-based, Differential context

### 2. Compact Formatter (UseCase)
- Format SelectedContext into minimal LLM-consumable text
- ~50 tokens per node, ~20 tokens per edge, ~100 tokens per community
- Include: key nodes, key edges, bridge nodes, suggested expansion paths

### 3. Conversation Memory (Domain + UseCase + Infrastructure)
- **ConversationNode**: Exchange node type with question, summary, relevant nodes, tokens used
- **Add to graph**: After each exchange, create node + link to referenced code nodes
- **Query history**: "What did we discuss about X?" via graph traversal

### 4. Token Budgeting (Domain)
- Pure budget allocation based on query complexity
- Dynamic ratios: Focused=0.1, Module=0.2, CrossModule=0.3, Architecture=0.4, Exploratory=0.25

### 5. Smart Router (Infrastructure — future, optional)
- Small local LLM integration (Ollama) for query classification
- Decides: local-only answer vs. send to big LLM with selected context
- This is Phase 3 — design for it now, implement later

### 6. MCP Tool Enhancement (Infrastructure)
- New MCP tool: `select_context` — takes query + budget → returns compact context string
- New MCP tool: `add_conversation` — stores exchange in graph
- New MCP tool: `conversation_history` — queries past exchanges
- Existing tools already provide the foundation

## Constraints
- **Haskell project**: All code must be Haskell with GHC 9.10
- **Clean architecture**: Domain has NO IO, UseCase has NO IO implementation, Infrastructure handles all side effects
- **Pure functions**: Context selection, scoring, formatting must be pure (Domain/UseCase)
- **PascalCase for types, camelCase for functions**
- **Explicit exports** on all modules
- **Type signatures** on all top-level definitions
- **Hspec + QuickCheck** for tests
- **Cabal build**: `cabal build && cabal test`
- **Existing types**: Reuse NodeId, Node, Edge, Confidence, CommunityMap, GodNode, Analysis, Relation, FileType where possible
- **No IO in Domain layer**: Conversation memory persistence goes in Infrastructure
- **Extend, don't replace**: Build on existing Query.hs, MCP.hs, Types.hs — don't refactor them

## Exit Criteria
- [ ] Domain types for context selection (ContextBudget, QueryComplexity, SelectedContext, ConversationNode)
- [ ] Pure context selection function using community-aware + relevance-weighted strategies
- [ ] Compact formatter producing LLM-optimized text output
- [ ] Token budgeting based on query complexity
- [ ] Conversation node types + persistence in infrastructure
- [ ] MCP tools: select_context, add_conversation, conversation_history
- [ ] Hspec tests for all pure functions
- [ ] `cabal build && cabal test` passes
- [ ] Documentation updated

## Progress
- [ ] Session initialized
- [ ] Tasks created
- [ ] Implementation complete