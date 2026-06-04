# Neo4j Sub-Graph Push Proposal

**Date**: 2026-04-25  
**Source**: Production pipeline run — 305,526 nodes, 380,906 edges, 3,739 files  
**Status**: Proposed  

---

## Executive Summary

The full Neo4j push generates ~990,000 Cypher statements for a 305k-node codebase, requiring 2–4 hours via sequential curl batches. This is impractical for large codebases and produces a graph that is slow to query in Neo4j.

This proposal introduces **three push modes** — Full, Sub-Graph, and Community-Only — with **Sub-Graph** as the recommended default for codebases with >10,000 nodes. Sub-Graph pushes each community's representative nodes (top-degree, bridges, entry points) with their intra-community edges, preserving file paths and structural detail while reducing statements by ~95%.

---

## Problem Statement

### Current Behavior

The `--neo4j --neo4j-push` pipeline generates Cypher `MERGE` statements for every node, edge, community, and BELONGS_TO relationship, then pushes them in batches of 50 via curl to Neo4j's transactional API.

### At Scale (305k nodes)

| Statement Type | Count |
|---|---|
| `:Node` MERGE | 305,526 |
| Edge MERGE | 380,906 |
| `:Community` MERGE | ~3,000 |
| `:BELONGS_TO` MERGE | 305,526 |
| **Total** | **~990,000** |
| Batches (50/batch) | ~19,800 |
| **Estimated push time** | **2–4 hours** |

### Issues

1. **Push time**: 2–4 hours for a single codebase is impractical
2. **Neo4j memory**: 990k entities require significant RAM and indexing
3. **Query performance**: Traversal across 680k+ entities is slow
4. **Relevance**: An LLM or agent navigating the graph doesn't need all 305k individual nodes — it needs the structural skeleton
5. **Data loss**: Community-only mode (the previous proposal) loses file paths, individual node labels, and intra-community edges entirely

---

## Proposed Solution

### Three Push Modes

```
data Neo4jPushMode
  = FullPush          -- All nodes + edges + communities (current behavior)
  | SubgraphPush      -- Communities + representative sub-graphs per community
  | CommunityPush     -- Communities + inter-community edges only
```

### Comparison

| | Full Push | **Sub-Graph Push** | Community Push |
|---|---|---|---|
| Nodes | All 305,526 | ~21,000 (representatives) | 0 |
| Edges | All 380,906 | ~14,000 (intra-community) | 0 |
| Communities | ~3,000 | ~3,000 | ~3,000 |
| BELONGS_TO | 305,526 | ~21,000 | 0 |
| Inter-community edges | (via BELONGS_TO) | ~5,000 (CONNECTED_TO) | ~5,000 (CONNECTED_TO) |
| **Total statements** | **~990,000** | **~64,000** | **~8,000** |
| **Push time (est.)** | 2–4 hours | ~30 seconds | ~5 seconds |
| **File paths preserved** | ✅ All | ✅ Representatives | ❌ None |
| **Node labels preserved** | ✅ All | ✅ Representatives | ❌ Only `top_members` text |
| **Intra-community edges** | ✅ All | ✅ Between representatives | ❌ None |
| **Query "where is X defined?"** | ✅ | ✅ If X is representative | ❌ |
| **Query "how do communities connect?"** | ✅ | ✅ | ✅ |

### Default Selection

```
nodes < 10,000   → FullPush (small graphs, no need to reduce)
nodes 10k–50k    → SubgraphPush (recommended default)
nodes > 50k      → SubgraphPush (strongly recommended; FullPush impractical)
```

Users can override with `--neo4j-push-mode`.

---

## Sub-Graph Push Design

### Node Selection: Representatives

Each community selects its **structurally important nodes** using pure domain logic:

```haskell
-- | Select representative nodes for each community.
-- Pure function, testable without IO.
--
-- Selection strategy per community:
--   1. Centroid: highest-degree node (the "main thing" this community is about)
--   2. Top-N by degree: next highest-degree nodes (most-connected, most referenced)
--   3. Bridge nodes: articulation points between communities
--   4. Deduplicate across communities (bridge nodes appear in both)
--
-- Parameters:
--   g        — the full graph
--   commMap  — community membership map
--   topN     — how many representatives per community (default: 7)
--   artPoints — articulation points (pre-computed, passed in)
selectRepresentatives
  :: Graph
  -> CommunityMap
  -> Int           -- ^ topN (how many per community)
  -> [NodeId]      -- ^ articulation points (bridge nodes)
  -> Map CommunityId [NodeId]
```

**Why these selection criteria:**

| Criterion | What it captures | Example |
|---|---|---|
| **Centroid** (highest degree) | The main concept this community revolves around | `parseConfig` in a Config community |
| **Top-N by degree** | Most-referenced functions/types | `loadYAML`, `validateSettings`, `defaultConfig` |
| **Bridge nodes** (articulation points) | Cross-community connectors | `defaultConfig` used by both Config and Pipeline |
| **Entry points** (file-level nodes) | Where someone would start reading | `src/Config/Parser.hs` |

**Default `topN = 7`** — gives meaningful detail per community while keeping total statements manageable:
- 3,000 communities × 7 representatives = 21,000 nodes
- Edges between those representatives ≈ 14,000 (only intra-community edges where both endpoints are representatives)
- Total ≈ 64,000 statements → ~30 seconds push

### What Gets Pushed to Neo4j

```cypher
-- Community node (same as current, plus top_members as string array)
MERGE (c:Community {id: $id})
ON CREATE SET c.label = $label, c.size = $size, c.cohesion = $cohesion

-- Representative node inside a community (new: has all node properties)
MERGE (n:Node {id: $id})
ON CREATE SET n.label = $label, n.file_type = $file_type,
  n.source_location = $source_location, n.source_url = $source_url,
  n.representative = true

-- Bridging node (connects communities — new flag)
MERGE (n:Node {id: $id})
ON CREATE SET n.label = $label, n.file_type = $file_type,
  n.source_location = $source_location, n.source_url = $source_url,
  n.bridge = true

-- Intra-community edge (between two representative/bridge nodes)
MATCH (src:Node {id: $source_id})
MATCH (tgt:Node {id: $target_id})
MERGE (src)-[:`imports` {confidence: $confidence, weight: $weight}]->(tgt)

-- Membership (representative/bridge node → community)
MATCH (n:Node {id: $node_id})
MATCH (c:Community {id: $community_id})
MERGE (n)-[:BELONGS_TO]->(c)

-- Inter-community connection (same as community-only push)
MATCH (c1:Community {id: $source_id})
MATCH (c2:Community {id: $target_id})
MERGE (c1)-[:CONNECTED_TO {edge_count: $edge_count, bridge_nodes: $bridge_nodes}]->(c2)
```

### Query Examples (Sub-Graph Mode)

**What does the Config Parser community look like inside?**
```cypher
MATCH (c:Community {label: 'Config Parser'})<-[:BELONGS_TO]-(n:Node)
RETURN n.label, n.source_location
```

**Where is parseConfig defined?**
```cypher
MATCH (n:Node {label: 'parseConfig'})
RETURN n.source_location, n.file_type
```

**How do Config and Pipeline communities connect?**
```cypher
MATCH (c1:Community)-[r:CONNECTED_TO]->(c2:Community)
WHERE c1.label CONTAINS 'Config' AND c2.label CONTAINS 'Pipeline'
RETURN c1.label, c2.label, r.edge_count, r.bridge_nodes
```

**Which communities have low cohesion?**
```cypher
MATCH (c:Community) WHERE c.cohesion < 0.3
RETURN c.label, c.size, c.cohesion ORDER BY c.cohesion
```

**What does the sub-graph inside community 3 look like?**
```cypher
MATCH (c:Community {id: 'community_3'})<-[:BELONGS_TO]-(n:Node)-[r]->(m:Node)
WHERE m:Node
RETURN n.label, type(r), m.label
```

### What Sub-Graph Mode Preserves vs Community-Only

| Data | Community-Only | Sub-Graph |
|---|---|---|
| Community label + cohesion + size | ✅ | ✅ |
| Inter-community edges + bridge nodes | ✅ | ✅ |
| Individual node labels | ❌ (only `top_members` text) | ✅ representative nodes |
| File paths (`source_location`) | ❌ | ✅ on representatives |
| Edges inside community | ❌ | ✅ between representatives |
| BELONGS_TO membership | ❌ | ✅ representatives → community |
| Node `:Bridge` flag | ❌ | ✅ |
| Node `:Representative` flag | ❌ | ✅ |

---

## Architecture

### Clean Architecture Layers

All pure logic stays in `Domain`. Only the HTTP push is in `Infrastructure`.

```
Domain (pure, no changes to existing)
  ├── Domain.Community
  │   └── selectRepresentatives  ← NEW (pure, testable)
  │
UseCase (pure orchestration)
  └── UseCase.Export
      └── branch on cfgNeo4jPushMode  ← MODIFIED
  └── UseCase.Pipeline
      └── log push mode + counts      ← MODIFIED
  │
Infrastructure (IO boundary)
  └── Infrastructure.Export.Neo4j
      ├── pushToNeo4j                 ← EXISTING (unchanged)
      ├── pushToNeo4jWithCommunities   ← EXISTING (unchanged)
      ├── pushSubgraphToNeo4j          ← NEW
      ├── pushCommunityGraphToNeo4j     ← NEW
      ├── generateSubgraphStatements   ← NEW (pure)
      └── generateCommunityOnlyStatements ← NEW (pure)
  │
Config
  ├── Neo4jPushMode(FullPush, SubgraphPush, CommunityPush)  ← NEW type
  └── cfgNeo4jPushMode, cfgNeo4jSubgraphSize                ← NEW fields
  │
Presentation
  └── app/Main.hs
      └── --neo4j-push-mode, --neo4j-subgraph-size          ← NEW flags
```

### New Type

```haskell
-- Domain.Types.Pipeline
data Neo4jPushMode
  = FullPush          -- All nodes + edges + communities (current behavior)
  | SubgraphPush      -- Communities + representative sub-graphs per community
  | CommunityPush     -- Communities + inter-community edges only
  deriving (Eq, Show, Read)

-- Added to PipelineConfig
  , cfgNeo4jPushMode   :: Neo4jPushMode   -- default: SubgraphPush for >10k nodes, FullPush otherwise
  , cfgNeo4jSubgraphSize :: Int            -- default: 7 (representatives per community)
```

### New Pure Functions (Domain.Community)

```haskell
-- | Select representative nodes for each community.
-- Pure function — no IO, fully testable.
selectRepresentatives
  :: Graph
  -> CommunityMap
  -> Int            -- ^ topN (representatives per community)
  -> [NodeId]       -- ^ articulation points (bridge nodes)
  -> Map CommunityId [NodeId]

-- | Filter edges to only those between nodes in the given set.
-- Pure function — used to generate intra-community sub-graph edges.
filterEdgesByNodeSet
  :: Set NodeId
  -> Map (NodeId, NodeId) Edge
  -> Map (NodeId, NodeId) Edge
```

### New Infrastructure Functions (Export.Neo4j)

```haskell
-- | Push community-level graph to Neo4j (fastest, no individual nodes).
pushCommunityGraphToNeo4j
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Text    -- ^ URI
  -> Text    -- ^ user
  -> Text    -- ^ password
  -> IO (Text, Int, Int)

-- | Push communities + representative sub-graphs to Neo4j.
pushSubgraphToNeo4j
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Int     -- ^ topN (representatives per community)
  -> Text    -- ^ URI
  -> Text    -- ^ user
  -> Text    -- ^ password
  -> IO (Text, Int, Int)

-- | Generate parameterized Cypher statements for community-only push.
-- Pure — can be tested without Neo4j.
generateCommunityOnlyStatements
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Map CommunityId Text
  -> [Aeson.Value]

-- | Generate parameterized Cypher statements for sub-graph push.
-- Pure — can be tested without Neo4j.
generateSubgraphStatements
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Map CommunityId Text
  -> Map CommunityId [NodeId]   -- ^ representatives per community
  -> Set NodeId                 -- ^ all representative/bridge node IDs
  -> [Aeson.Value]
```

All new generation functions are **pure** — they produce `[Aeson.Value]` that can be unit-tested without a running Neo4j instance. The existing `pushStatements` (batched curl push) is reused for the IO boundary.

---

## CLI Interface

```bash
# Default: auto-selects SubgraphPush for >10k nodes, FullPush for smaller
graphos . --neo4j --neo4j-push http://localhost:7474

# Explicit mode selection
graphos . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode full
graphos . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode subgraph
graphos . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode community

# Tune representatives per community (default: 7)
graphos . --neo4j --neo4j-push http://localhost:7474 --neo4j-subgraph-size 12

# Combine with resolution tuning
graphos . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode subgraph --resolution 1.5
```

### YAML Configuration

```yaml
# graphos.yaml
neo4j:
  uri: "http://localhost:7474"
  user: "neo4j"
  password: "graphos_dev"
  push_mode: "subgraph"      # full | subgraph | community
  subgraph_size: 7           # representatives per community
```

---

## Implementation Plan

### Phase 1: Core Types & Pure Logic (Domain + UseCase)

| # | File | Change | Est. |
|---|------|--------|------|
| 1.1 | `src/Graphos/Domain/Types/Pipeline.hs` | Add `Neo4jPushMode` type, `cfgNeo4jPushMode`, `cfgNeo4jSubgraphSize` to `PipelineConfig` | 10 min |
| 1.2 | `src/Graphos/Domain/Community.hs` | Add `selectRepresentatives`, `filterEdgesByNodeSet` | 30 min |
| 1.3 | `src/Graphos/Domain/Config.hs` | Add `Neo4jPushMode` to `Neo4jConfig`, parse from YAML | 15 min |
| 1.4 | `src/Graphos/Domain/Types.hs` | Re-export `Neo4jPushMode` | 2 min |

### Phase 2: Infrastructure (Neo4j Push)

| # | File | Change | Est. |
|---|------|--------|------|
| 2.1 | `src/Graphos/Infrastructure/Export/Neo4j.hs` | Add `pushCommunityGraphToNeo4j`, `pushSubgraphToNeo4j`, `generateCommunityOnlyStatements`, `generateSubgraphStatements` | 60 min |
| 2.2 | `src/Graphos/Infrastructure/Config.hs` | Parse `push_mode` and `subgraph_size` from YAML | 10 min |

### Phase 3: Orchestration (UseCase + CLI)

| # | File | Change | Est. |
|---|------|--------|------|
| 3.1 | `src/Graphos/UseCase/Export.hs` | Branch on `cfgNeo4jPushMode`, compute representatives, call appropriate push | 25 min |
| 3.2 | `src/Graphos/UseCase/Pipeline.hs` | Log push mode, representative count, estimated statements | 10 min |
| 3.3 | `app/Main.hs` | Add `--neo4j-push-mode` and `--neo4j-subgraph-size` CLI flags | 15 min |

### Phase 4: Tests

| # | File | Change | Est. |
|---|------|--------|------|
| 4.1 | `tests/Graphos/Domain/CommunitySpec.hs` | Test `selectRepresentatives`: centroid, topN, bridge nodes, deduplication | 30 min |
| 4.2 | `tests/Graphos/Infrastructure/Export/Neo4jSpec.hs` | Test `generateSubgraphStatements` and `generateCommunityOnlyStatements` (pure, no IO) | 30 min |
| 4.3 | Integration test | Manual: push to local Neo4j, verify all three modes | 15 min |

**Total estimate: ~4 hours**

---

## Testing Strategy

### Pure Unit Tests (no Neo4j needed)

```haskell
-- Domain.CommunitySpec
describe "selectRepresentatives" $ do
  it "selects centroid (highest-degree node) per community"
  it "selects topN nodes by degree"
  it "includes articulation points as bridge nodes"
  it "deduplicates across communities"
  it "respects topN limit"
  it "handles empty community"

-- Infrastructure.Export.Neo4jSpec
describe "generateSubgraphStatements" $ do
  it "generates MERGE statements for community nodes"
  it "generates MERGE statements for representative nodes with representative=true"
  it "generates MERGE statements for bridge nodes with bridge=true"
  it "generates BELONGS_TO edges for representatives"
  it "generates CONNECTED_TO edges between communities"
  it "generates intra-community edges between representatives"
  it "uses parameterized Cypher (no string injection)"
  it "includes source_location and file_type on nodes"

describe "generateCommunityOnlyStatements" $ do
  it "generates Community MERGE statements"
  it "generates CONNECTED_TO edges between communities"
  it "does NOT generate Node or BELONGS_TO statements"
```

### Integration Test (local Neo4j)

```bash
# Start Neo4j
docker compose up -d

# Clear database
curl -u neo4j:graphos_dev -X POST http://localhost:7474/db/neo4j/tx/commit \
  -H "Content-Type: application/json" \
  -d '{"statements":[{"statement":"MATCH (n) DETACH DELETE n"}]}'

# Run each mode
cabal run graphos -- . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode community
# Verify: MATCH (c:Community) RETURN count(c)

cabal run graphos -- . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode subgraph
# Verify: MATCH (n:Node) RETURN n.label, n.source_location LIMIT 5

cabal run graphos -- . --neo4j --neo4j-push http://localhost:7474 --neo4j-push-mode full
# Verify: MATCH (n:Node) RETURN count(n)
```

---

## Migration & Backward Compatibility

- **Default for <10k nodes**: `FullPush` — identical to current behavior, no breaking change
- **Default for ≥10k nodes**: `SubgraphPush` — new behavior, but explicit and documented
- **Existing flags unchanged**: `--neo4j` and `--neo4j-push` continue to work as before
- **New flags are additive**: `--neo4j-push-mode` and `--neo4j-subgraph-size` are optional
- **YAML config extends**: `push_mode` and `subgraph_size` fields are optional; defaults are sensible

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking existing `--neo4j-push` behavior | Low | High | Default mode preserves current behavior for <10k nodes |
| Representative selection misses important nodes | Medium | Low | `topN=7` captures most structure; users can increase via `--neo4j-subgraph-size` |
| Cypher `MERGE` conflicts between full and subgraph pushes | Low | Medium | Document: clear DB before switching modes |
| Neo4j property key collision (`representative`, `bridge`) | Low | Low | These are new boolean flags; won't conflict with existing schema |
| Push still slow for very large graphs (>500k nodes) | Low | Medium | Batch size can be increased from 50 to 500; parallel push implemented later |

---

## Future Considerations

1. **Parallel batch push**: Replace sequential `mapM` with `mapConcurrently` (8 parallel curl calls) — ~8x speedup for any mode
2. **Larger batch size**: Increase from 50 to 500-1000 statements per batch — fewer HTTP calls
3. **Bolt protocol**: Replace curl with a native Haskell Neo4j driver (e.g., `bolt` package) — eliminates process spawn overhead
4. **Incremental push**: Only push diff on subsequent runs (using checkpoint) instead of full DB replace
5. **Configurable node property inclusion**: Allow selecting which node properties to push (e.g., skip `source_url` for smaller payloads)
6. **Auto-clear before push**: Add `--neo4j-clear` flag to `MATCH (n) DETACH DELETE n` before pushing

---

## Decision

| Question | Recommendation |
|----------|---------------|
| Default push mode for <10k nodes? | `FullPush` (current behavior, no change) |
| Default push mode for ≥10k nodes? | `SubgraphPush` (recommended) |
| Default `topN` (representatives per community)? | 7 |
| Should we also implement Community-Only mode? | Yes — useful for ultra-fast LLM context loading |
| Implementation priority? | Phase 1+2 first (pure logic + push), Phase 3 (CLI), Phase 4 (tests) |