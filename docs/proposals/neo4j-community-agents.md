# Neo4j Community Agents Proposal

**Date**: 2026-04-27  
**Source**: Manual Neo4j community detection session — 175k nodes, GDS Louvain, TF-IDF labelling  
**Status**: Proposed  

---

## Executive Summary

Community labels in Neo4j currently require 12+ manual Cypher commands (GDS projection, Louvain, batch APOC processing, label generation). This proposal introduces **two OpenCode agents** that automate this end-to-end:

1. **Neo4jCommunityWriter** — Detects communities and writes labels to Neo4j via HTTP Cypher
2. **Neo4jCommunityReader** — Retrieves community data from Neo4j via HTTP Cypher queries

Both agents are **OpenCode subagents** that call Neo4j's HTTP transactional API directly. No Graphos CLI, no Haskell pipeline — just Cypher over HTTP.

---

## Problem Statement

### What We Did Manually (Today)

| Step | Cypher / Command | Duration |
|------|-----------------|----------|
| 1. Drop old GDS projection | `CALL gds.graph.drop` | instant |
| 2. Create GDS projection | `CALL gds.graph.project('graphos', 'Node', ...)` | ~5s |
| 3. Run Louvain + write communityId | `CALL gds.louvain.write('graphos', {writeProperty: 'communityId'})` | ~30s |
| 4. Assign singleton communityIds to isolated nodes | `MATCH (n:Node) WHERE n.communityId IS NULL SET n.communityId = ...` | ~10s |
| 5. Create Community nodes with size | `MERGE (c:Community {communityId: cid})` | ~15s |
| 6. Compute TF-IDF labels (connected communities) | Complex APOC text processing | ~30s |
| 7. Label remaining communities | APOC text processing on member labels | ~15s |
| 8. Label orphan communities | Default names | instant |
| 9. Compute cohesion scores | `2 * internal_edges / (size * (size - 1))` | ~10s |
| 10. Create BELONGS_TO edges | `apoc.periodic.iterate` (5k batches) | ~20s |
| 11. Create CONNECTED_TO inter-community edges | Aggregation + MERGE | ~10s |
| 12. Drop GDS projection | `CALL gds.graph.drop` | instant |
| **Total** | | **~2.5 min** |

This is repetitive, error-prone, and requires Neo4j expertise. It should be automated.

### What's Missing After Communities Are Created

Once communities exist in Neo4j, there's **no way to query them** from the OpenCode context system. The Graphos knowledge graph lives in JSON files; the Neo4j graph is a separate world. We need an agent that can **retrieve community data from Neo4j** and feed it into LLM context.

---

## Proposed Solution: Two Agents

### Agent 1: Neo4jCommunityWriter

**Purpose**: Detect communities in an existing Neo4j graph and write labels, edges, and metadata.

**Trigger**: User says "add community labels" or "detect communities in Neo4j"

**Input**: Neo4j connection config (URI, user, password), optional resolution/min-size

**Output**: Community statistics (count, node coverage, modularity, label samples)

```
┌──────────────────────────────────────────────────┐
│           Neo4jCommunityWriter                    │
│                                                    │
│  1. Project GDS graph                             │
│  2. Run Louvain → write communityId to nodes      │
│  3. Assign singleton IDs to isolated nodes         │
│  4. Create Community nodes (id, size)             │
│  5. Compute TF-IDF labels via Cypher + APOC       │
│  6. Compute cohesion scores                        │
│  7. Create BELONGS_TO edges (batched)              │
│  8. Create CONNECTED_TO inter-community edges      │
│  9. Drop GDS projection                            │
│  10. Return stats summary                          │
│                                                    │
│  All steps: HTTP POST /db/neo4j/tx/commit          │
│  Auth: Basic auth (user:password)                 │
│  Format: {"statements": [{"statement": "...",     │
│           "parameters": {...}}]}                  │
└──────────────────────────────────────────────────┘
```

### Agent 2: Neo4jCommunityReader

**Purpose**: Retrieve community data from Neo4j for LLM context, analysis, or display.

**Trigger**: User asks a question about communities, wants to explore the Neo4j graph, or needs context for a code question.

**Input**: Cypher query or natural language description of what to find.

**Output**: Structured community data (labels, members, connections, cohesion).

```
┌──────────────────────────────────────────────────┐
│           Neo4jCommunityReader                    │
│                                                    │
│  Query types:                                      │
│  ─ "List communities" → MATCH (c:Community)      │
│  ─ "Show community X" → MATCH (c:Community ...)  │
│  ─ "Community of node Y" → via BELONGS_TO        │
│  ─ "Connected communities" → via CONNECTED_TO    │
│  ─ "Low cohesion communities" → WHERE c.cohesion │
│  ─ "Members of community Z" → via BELONGS_TO      │
│  ─ Arbitrary Cypher → direct execution            │
│                                                    │
│  All queries: HTTP POST /db/neo4j/tx/commit       │
│  Results: parsed from Neo4j JSON response         │
│  Format: markdown tables / structured data        │
└──────────────────────────────────────────────────┘
```

---

## Architecture

### Where These Live

Both agents are **OpenCode subagent configurations** — not Haskell code. They are declarative agent definitions that the OpenCode system can invoke. They communicate with Neo4j **exclusively via HTTP** (the Neo4j transactional HTTP API).

```
.opencode/
├── agents/
│   ├── neo4j-community-writer.md    -- Agent definition + Cypher templates
│   └── neo4j-community-reader.md    -- Agent definition + query templates
```

### Neo4j HTTP API

Both agents use the **Neo4j Transactional HTTP API**:

```
POST http://localhost:7474/db/neo4j/tx/commit
Authorization: Basic <base64(neo4j:graphos_dev)>
Content-Type: application/json

{
  "statements": [
    {
      "statement": "MATCH (n:Node) WHERE n.communityId IS NOT NULL RETURN count(n)",
      "parameters": {}
    }
  ]
}
```

**Response format:**
```json
{
  "results": [{
    "columns": ["count(n)"],
    "data": [{"row": [175561], "meta": [null]}]
  }],
  "errors": []
}
```

### Dependencies

| Dependency | Status | Why Needed |
|-----------|--------|------------|
| Neo4j GDS plugin | Already in docker-compose | Louvain community detection |
| APOC plugin | Already in docker-compose | Text processing for TF-IDF labels |
| Neo4j HTTP API | Built-in | All queries and mutations |
| Basic auth | Built-in | Authentication |

---

## Agent 1: Neo4jCommunityWriter — Detailed Design

### Configuration

```yaml
# .opencode/agents/neo4j-community-writer.md
neo4j:
  uri: "http://localhost:7474"
  user: "neo4j"
  password: "graphos_dev"

community_detection:
  algorithm: "louvain"          # louvain | wcc
  resolution: 1.0               # Louvain gamma (higher = fewer, larger communities)
  max_levels: 10                # Max Louvain iteration levels
  tolerance: 0.0001             # Convergence tolerance
  min_community_size: 3         # Communities smaller than this get merged

labeling:
  method: "tfidf"               # tfidf (Cypher-based) | llm (future)
  max_words: 3                  # Words per community label
  stop_words: "default"         # Built-in stop word list (shared with Domain.Community.Label)

relationships:
  project_undirected: true      # Treat contains/references as undirected
  relationship_types:           # Which relationship types to include in community detection
    - "contains"
    - "references"
```

### Cypher Templates (Step-by-Step)

#### Step 1: Clean up any existing GDS projection

```cypher
// Check if projection exists, drop if so
CALL gds.graph.list() YIELD graphName
WHERE graphName = 'graphos'
CALL gds.graph.drop(graphName) YIELD graphName AS dropped
RETURN dropped
```

#### Step 2: Project graph for GDS

```cypher
CALL gds.graph.project('graphos', 'Node', {
  contains: {orientation: 'UNDIRECTED'},
  references: {orientation: 'UNDIRECTED'}
}) YIELD graphName, nodeCount, relationshipCount
RETURN graphName, nodeCount, relationshipCount
```

#### Step 3: Run Louvain and write communityId

```cypher
CALL gds.louvain.write('graphos', {
  writeProperty: 'communityId',
  maxLevels: 10,
  tolerance: 0.0001,
  concurrency: 4
}) YIELD communityCount, ranLevels, modularity, nodePropertiesWritten
RETURN communityCount, ranLevels, modularity, nodePropertiesWritten
```

#### Step 4: Assign singleton communityIds to isolated nodes

```cypher
// Nodes that weren't in the projected subgraph (no edges)
MATCH (n:Node) WHERE n.communityId IS NULL
WITH collect(n) AS orphans
UNWIND range(0, size(orphans)-1) AS i
WITH orphans[i] AS node, 300000 + i AS cid
SET node.communityId = cid
RETURN count(*) AS orphansAssigned
```

#### Step 5: Create Community nodes with size

```cypher
MATCH (n:Node) WHERE n.communityId IS NOT NULL
WITH n.communityId AS cid, count(n) AS size
MERGE (c:Community {communityId: cid})
ON CREATE SET c.id = 'community_' + cid, c.size = size
RETURN count(c) AS communitiesCreated
```

#### Step 6a: Compute TF-IDF labels (connected communities)

```cypher
// For communities with edges: use neighbor-connected node labels
MATCH (n:Node)-[:contains|references]-(m:Node)
WHERE n.communityId IS NOT NULL
WITH n.communityId AS cid, collect(DISTINCT n.label) AS labels, count(DISTINCT n) AS size
WITH cid, size,
     apoc.text.join(labels, ' ') AS allLabels
WITH cid, size,
     apoc.text.replace(
       apoc.text.replace(
         apoc.text.replace(allLabels, '[^a-zA-Z0-9_\\s]', ' '),
         '\\s+', ' '),
       '^\\s+|\\s+$', '') AS cleaned
WITH cid, size, split(cleaned, ' ') AS words
WITH cid, size, [w IN words WHERE size(w) > 1
  AND NOT toLower(w) IN [/* stop words list */]] AS filtered
UNWIND filtered AS w
WITH cid, size, toLower(w) AS word
WITH cid, size, word, count(*) AS freq
WITH cid, size, collect({word: word, freq: freq}) AS wordFreqs
WITH cid, size, apoc.coll.sortMaps(wordFreqs, 'freq') AS sorted
WITH cid, size, sorted[0..3] AS top3
WITH cid, size,
     apoc.text.join([item IN top3 | apoc.text.capitalizeAll(item.word)], ' ') AS label
MATCH (c:Community {communityId: cid})
SET c.label = label
RETURN count(c) AS updated
```

#### Step 6b: Label remaining communities (no edges, use member labels)

Same pattern but starting from `MATCH (n:Node) WHERE n.communityId IS NOT NULL` instead of matching via edges.

#### Step 7: Compute cohesion scores

```cypher
MATCH (n:Node)-[r:contains|references]-(m:Node)
WHERE n.communityId IS NOT NULL AND m.communityId IS NOT NULL
  AND n.communityId = m.communityId AND id(n) < id(m)
WITH n.communityId AS cid, count(*) AS internalEdges
MATCH (c:Community {communityId: cid})
WITH c, internalEdges, c.size AS sz
WITH c, CASE
  WHEN sz <= 1 THEN 0.0
  WHEN internalEdges * 2.0 / (sz * (sz - 1.0)) > 1.0 THEN 1.0
  ELSE round(internalEdges * 2.0 / (sz * (sz - 1.0)) * 1000) / 1000.0
END AS cohesion
SET c.cohesion = cohesion
RETURN count(c) AS updated
```

#### Step 8: Create BELONGS_TO edges (batched)

```cypher
CALL apoc.periodic.iterate(
  'MATCH (n:Node) WHERE n.communityId IS NOT NULL RETURN n',
  'MATCH (c:Community {communityId: n.communityId}) MERGE (n)-[:BELONGS_TO]->(c)',
  {batchSize: 5000, parallel: true}
) YIELD batches, total, committedOperations, failedOperations
RETURN batches, total, committedOperations, failedOperations
```

#### Step 9: Create CONNECTED_TO inter-community edges

```cypher
MATCH (n:Node)-[r:contains|references]-(m:Node)
WHERE n.communityId IS NOT NULL AND m.communityId IS NOT NULL
  AND n.communityId <> m.communityId AND id(n) < id(m)
WITH n.communityId AS srcCid, m.communityId AS tgtCid,
     count(*) AS edgeCount,
     collect(DISTINCT n.label)[0..5] AS bridgeNodes
MATCH (src:Community {communityId: srcCid}), (tgt:Community {communityId: tgtCid})
MERGE (src)-[:CONNECTED_TO {edge_count: edgeCount, bridge_nodes: bridgeNodes}]->(tgt)
RETURN count(*) AS connectionsCreated
```

#### Step 10: Cleanup — drop GDS projection

```cypher
CALL gds.graph.drop('graphos') YIELD graphName
RETURN graphName + ' dropped' AS result
```

#### Step 11: Return summary stats

```cypher
MATCH (c:Community) RETURN
  count(c) AS communities,
  avg(c.cohesion) AS avgCohesion,
  max(c.size) AS maxSize,
  min(c.size) AS minSize
```

```cypher
MATCH ()-[r:BELONGS_TO]->() RETURN count(r) AS belongsTo
```

```cypher
MATCH ()-[r:CONNECTED_TO]->() RETURN count(r) AS connectedTo
```

### Error Handling

| Step | Failure Mode | Recovery |
|------|-------------|----------|
| GDS project | Projection already exists | Drop first, retry |
| Louvain | Graph too large (OOM) | Log error, suggest `--resolution 2.0` |
| Singleton assignment | No orphaned nodes | Skip step (0 orphans) |
| TF-IDF labels | APOC not available | Fall back to simple top-3 word frequency |
| BELONGS_TO | Batch failure | Retry with smaller batch size |
| CONNECTED_TO | No inter-community edges | Skip (all nodes in 1 community) |
| Cleanup | Drop fails | Warn but continue (projection auto-expires) |

---

## Agent 2: Neo4jCommunityReader — Detailed Design

### Configuration

```yaml
# .opencode/agents/neo4j-community-reader.md
neo4j:
  uri: "http://localhost:7474"
  user: "neo4j"
  password: "graphos_dev"
```

### Query Templates

The reader agent translates natural-language intents into Cypher queries:

| Intent | Cypher Query |
|--------|-------------|
| List all communities | `MATCH (c:Community) RETURN c.id, c.label, c.size, c.cohesion ORDER BY c.size DESC` |
| Show community by label | `MATCH (c:Community) WHERE c.label CONTAINS $term RETURN c.*` |
| Get community members | `MATCH (n:Node)-[:BELONGS_TO]->(c:Community {id: $cid}) RETURN n.id, n.label, n.file_type` |
| Get community of a node | `MATCH (n:Node {id: $nodeId})-[:BELONGS_TO]->(c:Community) RETURN c.*` |
| Connected communities | `MATCH (c1:Community)-[r:CONNECTED_TO]->(c2:Community) RETURN c1.label, c2.label, r.edge_count, r.bridge_nodes` |
| Low cohesion communities | `MATCH (c:Community) WHERE c.cohesion < $threshold RETURN c.* ORDER BY c.cohesion` |
| Largest communities | `MATCH (c:Community) RETURN c.* ORDER BY c.size DESC LIMIT $n` |
| Bridge nodes between communities | `MATCH (c1:Community)-[r:CONNECTED_TO]->(c2:Community) WHERE c1.label CONTAINS $term1 AND c2.label CONTAINS $term2 RETURN r.bridge_nodes` |
| Node neighborhood with community | `MATCH (n:Node {id: $nodeId})-[r]-(m:Node) RETURN n.id, n.communityId, m.id, m.communityId, type(r)` |
| Community graph overview | `MATCH (c:Community) RETURN c.id, c.label, c.size, c.cohesion, [(c)<-[:BELONGS_TO]-(n:Node) \| n.label][0..5] AS top_members ORDER BY c.size DESC LIMIT 20` |
| Arbitrary Cypher | Direct execution (trusted environment) |

### Response Formatting

The reader formats Neo4j JSON responses into readable markdown:

```
## Communities (top 10 by size)

| Community | Label | Size | Cohesion |
|-----------|-------|------|----------|
| community_76988 | 04 2026 Request | 4,656 | 0.000 |
| community_68106 | Https Org Resolved | 4,427 | 0.001 |
| community_92830 | Block Prd Phase | 1,876 | 0.001 |

### Connections
- **04 2026 Request** ↔ **Https Org Resolved** (23 edges, bridges: [doc_url, request_handler])
- ...
```

---

## Agent Interaction Flow

### Typical Session

```
User: "Add community labels to Neo4j"
  ↓
OpenCode → Neo4jCommunityWriter
  → HTTP POST: project GDS graph
  → HTTP POST: Louvain detection
  → HTTP POST: create Community nodes
  → HTTP POST: TF-IDF labels
  → HTTP POST: cohesion scores
  → HTTP POST: BELONGS_TO edges
  → HTTP POST: CONNECTED_TO edges
  → HTTP POST: drop GDS projection
  → Return summary: "3,161 communities, modularity 0.997, 176k nodes assigned"
  ↓
User: "Show me the largest communities"
  ↓
OpenCode → Neo4jCommunityReader
  → HTTP POST: MATCH (c:Community) RETURN c.* ORDER BY c.size DESC LIMIT 10
  → Format as markdown table
  ↓
User: "What's in the Config Parser community?"
  ↓
OpenCode → Neo4jCommunityReader
  → HTTP POST: MATCH (n)-[:BELONGS_TO]->(c:Community) WHERE c.label CONTAINS 'Config' RETURN n.id, n.label
  → Format as list
  ↓
User: "Which communities connect to Config Parser?"
  ↓
OpenCode → Neo4jCommunityReader
  → HTTP POST: MATCH (c1:Community)-[:CONNECTED_TO]->(c2) WHERE c1.label CONTAINS 'Config' RETURN c2.label, ...
  → Format connections
```

### Idempotency

The writer agent is **fully idempotent** — all writes use `MERGE`, making it safe to run multiple times:
- `MERGE (c:Community {communityId: cid})` — creates or matches
- `MERGE (n)-[:BELONGS_TO]->(c)` — creates or matches
- `MERGE (src)-[:CONNECTED_TO]->(tgt)` — creates or matches

Re-running the writer simply **recalculates** community assignments (which may shift if the graph changed).

---

## HTTP API Details

### Authentication

Both agents authenticate via HTTP Basic Auth:

```
Authorization: Basic base64(neo4j:graphos_dev)
```

Configurable via agent config (not hardcoded).

### Request Format

```json
POST /db/neo4j/tx/commit HTTP/1.1
Host: localhost:7474
Authorization: Basic bmVvNGo6Z3JhcGhvc19kZXY=
Content-Type: application/json
Accept: application/json

{
  "statements": [
    {
      "statement": "MATCH (c:Community) RETURN c.label, c.size ORDER BY c.size DESC LIMIT $limit",
      "parameters": {"limit": 10}
    }
  ]
}
```

### Response Parsing

```json
{
  "results": [{
    "columns": ["c.label", "c.size"],
    "data": [
      {"row": ["04 2026 Request", 4656]},
      {"row": ["Https Org Resolved", 4427]}
    ]
  }],
  "errors": []
}
```

Agent extracts `results[0].data[*].row` and maps to `results[0].columns`.

### Error Detection

```json
{
  "errors": [{
    "code": "Neo.ClientError.Statement.SyntaxError",
    "message": "Invalid input..."
  }]
}
```

Agent checks `errors` array — if non-empty, report and stop.

### Batch Limits

- Max statements per request: **50** (Neo4j default limit)
- For large operations (BELONGS_TO edges), use APOC `apoc.periodic.iterate` which handles batching internally
- Timeout: **300 seconds** per request (matching existing `pushBatch` in Neo4j.hs)

---

## What We Already Have (Verified)

This proposal is based on **proven Cypher queries** that we ran manually today:

| Step | Result | Verified |
|------|--------|----------|
| GDS projection | 175,561 nodes, 425,542 relationships | ✅ |
| Louvain detection | 3,161 communities, modularity 0.997 | ✅ |
| Community nodes with labels | 3,833 communities, all labeled | ✅ |
| Cohesion scores | avg 0.144, computed for all | ✅ |
| BELONGS_TO edges | 176,233 created, 0 failures | ✅ |
| CONNECTED_TO edges | 746 inter-community connections | ✅ |
| Idempotency | MERGE everywhere, safe to re-run | ✅ |

---

## Implementation Plan

### Phase 1: Agent Definition Files

| # | File | Content | Est. |
|---|------|---------|------|
| 1.1 | `.opencode/agents/neo4j-community-writer.md` | Agent definition, Cypher templates, error handling, config | 1 hr |
| 1.2 | `.opencode/agents/neo4j-community-reader.md` | Agent definition, query templates, response formatting | 1 hr |

### Phase 2: OpenCode Integration

| # | Task | Est. |
|---|------|------|
| 2.1 | Register both agents in OpenCode agent registry | 30 min |
| 2.2 | Wire Neo4jCommunityWriter to "add communities" intent | 30 min |
| 2.3 | Wire Neo4jCommunityReader to community query intents | 30 min |
| 2.4 | Test end-to-end: writer → reader flow | 1 hr |

### Phase 3: Haskell Infrastructure (Optional — Future)

| # | Task | Est. |
|---|------|------|
| 3.1 | `Domain/Neo4jCommunity/Types.hs` — Pure types | 30 min |
| 3.2 | `UseCase/Neo4jCommunity.hs` — Pure orchestration | 1 hr |
| 3.3 | `Infrastructure/Neo4jCommunity/Agent.hs` — HTTP + Cypher execution | 2 hr |
| 3.4 | `app/Main.hs` — `graphos neo4j-communities` CLI command | 1 hr |
| 3.5 | Tests | 2 hr |

**Phase 1+2 total: ~4 hours** (agent definitions only, no Haskell changes)  
**Phase 3 total: ~7 hours** (optional, for standalone CLI command)

---

## Comparison: Agent-Only vs Haskell CLI

| Aspect | Agent (Phase 1+2) | Haskell CLI (Phase 3) |
|--------|-------------------|----------------------|
| Speed to implement | Fast (markdown agent configs) | Slow (Haskell modules) |
| Flexibility | High (edit Cypher templates anytime) | Low (requires recompile) |
| Testability | Manual (run against Neo4j) | Full (Hspec + QuickCheck) |
| Offline | Requires Neo4j running | Same |
| Batch processing | Built into APOC Cypher | Custom Haskell batching |
| Error recovery | Agent can retry/adapt | Hardcoded strategy |
| Reusability | Any OpenCode session | Only via `graphos` CLI |
| Long-term maintenance | Cypher templates | Haskell code + types |

**Recommendation**: Start with Phase 1+2 (agents). If the agent approach proves insufficient (performance, reliability), implement Phase 3 as a promoted, compiled version.

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| GDS plugin not available | Low | High | Check `gds.version()` before starting, clear error message |
| APOC not available | Low | Medium | Fall back to simple frequency-based labels (no TF-IDF) |
| Neo4j not running | Medium | High | Check health endpoint before starting, suggest `docker compose up` |
| Large graph OOM on Louvain | Low | High | Set `concurrency: 1`, lower `maxLevels`, or suggest increasing Neo4j heap |
| Agent HTTP timeout | Low | Medium | Increase to 300s, use APOC batch processing |
| Community IDs shift on re-run | Expected | Low | Document: communities are not stable across runs (Louvain is stochastic) |

---

## Future Considerations

1. **Incremental community update**: Instead of re-running Louvain on the full graph, update only for changed nodes
2. **LLM labeling**: Use `--label` mode with OpenAI/Ollama to generate human-readable community names (replacing TF-IDF)
3. **GDS Leiden**: Switch from Louvain to Leiden algorithm (better quality, guaranteed well-connected communities) — requires GDS 2.5+
4. **Streaming writer**: Push community data as it's detected (no need to wait for all communities)
5. **Community diff**: Compare community assignments across runs to track structural changes
6. **Reader → context integration**: Feed reader results into `graphos select-context` for LLM query augmentation