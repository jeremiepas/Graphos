# 03 — Architecture

Target architecture for Graphos as a full memory agent.

## Current Architecture

```
                          ┌──────────────┐
                          │   CLI / MCP   │
                          └──────┬───────┘
                                 │
                    ┌────────────▼────────────┐
                    │     UseCase Layer        │
                    │  (Pipeline, Query,       │
                    │   SelectContext,          │
                    │   Conversation)            │
                    └────────────┬──────────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                   │                   │
    ┌─────────▼──────┐  ┌───────▼────────┐  ┌───────▼────────┐
    │   Domain Layer   │  │  Infrastructure │  │   Graph Data    │
    │  Types, Graph,   │  │  LSP, FS, LLM,  │  │   FGL, Index    │
    │  Community,      │  │  Server, Export  │  │                 │
    │  Context,        │  │                  │  │                 │
    │  Analysis        │  │                  │  │                 │
    └──────────────────┘  └──────────────────┘  └────────────────┘
```

**Key issue**: The graph is immutable after pipeline. MCP reads it but can't mutate it.

## Target Architecture

```
                          ┌──────────────────────────────────┐
                          │        Memory Agent API           │
                          │  (MCP Server + future REST/gRPC)  │
                          └────────────┬─────────────────────┘
                                       │
                    ┌──────────────────▼──────────────────┐
                    │         Memory Agent Core            │
                    │                                      │
                    │  ┌────────────────────────────────┐  │
                    │  │     Mutable Graph State         │  │
                    │  │     (TVar Graph + TVar Index)    │  │
                    │  │     + ConversationStore         │  │
                    │  └────────────┬───────────────────┘  │
                    │               │                      │
                    │  ┌────────────▼───────────────────┐  │
                    │  │      Agent Operations           │  │
                    │  │                                 │  │
                    │  │  ┌──────────┐ ┌─────────────┐  │  │
                    │  │  │ Context  │ │ Semantic    │  │  │
                    │  │  │ Selector │ │ Search      │  │  │
                    │  │  └──────────┘ └─────────────┘  │  │
                    │  │  ┌──────────┐ ┌─────────────┐  │  │
                    │  │  │Memory    │ │ Temporal    │  │  │
                    │  │  │Manager   │ │ Scorer      │  │  │
                    │  │  └──────────┘ └─────────────┘  │  │
                    │  │  ┌──────────┐ ┌─────────────┐  │  │
                    │  │  │ LLM      │ │ Incremental │  │  │
                    │  │  │ Summarize│ │ Updater     │  │  │
                    │  │  └──────────┘ └─────────────┘  │  │
                    │  └────────────────────────────────┘  │
                    │                                      │
                    │  ┌────────────────────────────────┐  │
                    │  │     Persistence Layer            │  │
                    │  │  (graph.json + memory/ dir +     │  │
                    │  │   periodic snapshots)            │  │
                    │  └────────────────────────────────┘  │
                    └──────────────────────────────────────┘
                                       │
                    ┌──────────────────┼──────────────────┐
                    │                  │                   │
          ┌────────▼─────┐ ┌─────────▼──────┐ ┌────────▼──────┐
          │ Domain Layer  │ │ Infrastructure  │ │ Embeddings    │
          │ (pure types)  │ │ (LSP, FS, LLM) │ │ (optional)    │
          └──────────────┘ └────────────────┘ └───────────────┘
```

## Key Design Decisions

### 1. Mutable Graph State

**Decision**: Use `TVar Graph` in the MCP server instead of immutable `Graph` value.

**Rationale**:
- Conversations added via `add_conversation` must be visible immediately
- Multiple MCP requests in the same session need consistent state
- STM provides composable concurrent access

**Implementation**:
```haskell
data MemoryAgentState = MemoryAgentState
  { masGraph      :: TVar Graph          -- Mutable knowledge graph
  , masIndex      :: TVar GraphIndex      -- Mutable inverted index
  , masCommMap    :: TVar CommunityMap     -- Mutable communities
  , masAnalysis   :: TVar Analysis        -- Mutable analysis results
  , masConvs      :: TVar [ConversationNode] -- In-memory conversations
  , masDirty      :: TVar Bool             -- Graph changed since last save?
  }
```

### 2. Embedding Integration

**Decision**: Support both local and API-based embeddings. Start with API, add local later.

**Embedding providers**:
```haskell
data EmbeddingProvider
  = OpenAIEmbeddings          -- text-embedding-3-small
  | LocalModel FilePath       -- sentence-transformers via subprocess
  | OllamaEmbeddings Text     -- Ollama API endpoint
  | NoEmbeddings              -- substring matching only (current)
  ```

**Storage**: Embeddings stored in `Node.nodeEmbedding` field and a separate `embeddings.json` file for large graphs.

**Search**: Hybrid scoring — substring match + embedding cosine similarity + community proximity.

### 3. LLM Summarization

**Decision**: Reuse existing `Infrastructure.LLM.OpenAI` module. Add fallback for no-LLM case.

**Flow**:
```
User asks question → LLM answers → Memory Agent creates ConversationNode:
  1. question = user's original question
  2. summary = LLM-generated summary (or question text if no LLM)
  3. relevant_nodes = IDs extracted from context selection
  4. timestamp = current time
  5. tokens_used = token count from LLM response
```

### 4. Temporal Scoring

**Decision**: Add exponential decay to context selection scores.

**Formula**:
```
recencyBoost(ts, now) = exp(-λ * (now - ts) / halfLife)
```

Where:
- `halfLife = 7 days` (configurable)
- `λ = ln(2) / halfLife`
- Maximum boost: 2.0 (conversations from last hour)
- Minimum boost: 0.1 (conversations older than 30 days)

### 5. Incremental Community Update

**Decision**: Don't re-run full Leiden on every node addition. Use local community adjustment.

**Approach**:
- New node: assign to the community of its most-connected neighbor
- New edge: if it connects two communities, check if it's a bridge
- Periodic re-clustering: after N mutations or time interval

```haskell
-- Assign new node to community of most-connected neighbor
assignToCommunity :: NodeId -> Graph -> CommunityMap -> CommunityId
assignToCommunity nid g commMap =
  let nbs = Set.toList (neighbors g nid)
      neighborCommunities = [cid | nb <- nbs
                                  , Just cid <- [Map.lookup nb nodeCommMap]]
      mostCommon = head (maximumBySize neighborCommunities)
  in fromMaybe 0 mostCommon
```

### 6. Cross-Session Persistence

**Decision**: Write back to `graph.json` on shutdown + after every N mutations.

**Flow**:
```
1. Startup: load graph.json + memory/ → merge into initial Graph
2. Runtime: mutations update TVar + write memory/ files immediately
3. Periodic: every 10 mutations, snap-shot graph.json
4. Shutdown: final snapshot of graph.json
```

### 7. MCP Tool Additions

New tools for the MCP server:

| Tool | Purpose | Priority |
|------|---------|----------|
| `add_conversation` | ✅ Already exists | — |
| `add_node` | Insert a single node | Medium |
| `add_edge` | Insert an edge | Medium |
| `remove_node` | Remove node + edges | Low |
| `update_node` | Update node metadata | Low |
| `search_semantic` | Embedding-based search | High |
| `select_context` | ✅ Already exists | — |
| `conversation_history` | ✅ Already exists | — |

## Module Changes

### New Modules

| Module | Layer | Purpose |
|--------|-------|---------|
| `Domain.Embedding` | Domain | Embedding vector type, cosine similarity |
| `UseCase.SemanticSearch` | UseCase | Hybrid search (text + embeddings) |
| `UseCase.Temporal` | UseCase | Time-based relevance scoring |
| `UseCase.MemoryAgent` | UseCase | Memory agent orchestrator |
| `Infrastructure.Embedding.OpenAI` | Infra | OpenAI embeddings API client |
| `Infrastructure.Embedding.Local` | Infra | Local sentence-transformers |

### Modified Modules

| Module | Change |
|--------|--------|
| `Domain.Types.Node` | Add `nodeEmbedding :: Maybe EmbeddingVector` |
| `Domain.Context` | Add temporal fields to `SelectedContext` |
| `UseCase.SelectContext` | Add temporal scoring, hybrid search |
| `UseCase.Conversation` | Add LLM summarization integration |
| `Infrastructure.Server.MCP` | Mutable `TVar` state, new tools |
| `Infrastructure.FileSystem.Conversation` | Merge conversations into graph |

## Data Flow

### Current Data Flow (Read-Only)

```
                          ┌─────────────┐
                          │ graph.json   │
                          └──────┬──────┘
                                 │ load
                          ┌──────▼──────┐
                          │ Immutable    │
                          │ Graph Value  │
                          └──────┬──────┘
                                 │ read-only
                    ┌────────────▼────────────┐
                    │    MCP Server              │
                    │  (11 read tools)           │
                    └────────────────────────────┘
```

### Target Data Flow (Read-Write)

```
                          ┌─────────────┐
                 ┌───────┤ graph.json   ├───────┐
                 │ load  └──────────────┘  save  │
                 │                               │
          ┌──────▼──────┐               ┌───────▼───────┐
          │ TVar Graph  │◄──────────────┤ Periodic      │
          │ + Index     │               │ Snapshot      │
          └──────┬──────┘               └───────────────┘
                 │ STM
    ┌────────────▼────────────────────┐
    │         Memory Agent            │
    │                                 │
    │  select_context  ◄──── Graph   │
    │  add_conversation ────► Graph  │
    │  add_node         ────► Graph  │
    │  search_semantic  ◄──── Embed  │
    │  conversation_history ◄ Disk+G │
    └────────────┬────────────────────┘
                 │
          ┌──────▼──────┐
          │  memory/ dir │ (disk persistence)
          └──────────────┘
```