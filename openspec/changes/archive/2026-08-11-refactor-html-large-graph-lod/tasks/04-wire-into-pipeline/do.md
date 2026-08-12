<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Wire join + aggregates into Pipeline.hs — DO

**Task slug**: `04-wire-into-pipeline`
**Attempt**: 1
**Status**: pending

## Summary

Wire `joinCommunitiesToNodes` and `computeCommunityAggregates` into the export pipeline, add the `epWriteCommunityAggregates` port method, implement SQLite export, and add COOP/COEP headers to the static server.

## Detail

### Concrete Changes

**File: `src/Graphos/UseCase/Port/ExportPort.hs`**

Add to the `ExportPort` class:
```haskell
, epWriteCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()
```

**File: `src/Graphos/UseCase/Pipeline.hs`**

After the re-cluster step (~line 263) and before `epWriteNodes` (~line 269):
```haskell
-- Join communities to nodes
let enrichedGraph'' = joinCommunitiesToNodes enrichedGraph' finalComm

-- Compute aggregates
let artPoints = -- from analysis
    mLabels = -- from LLM labeling step
    aggregates = computeCommunityAggregates enrichedGraph'' finalComm finalCohes artPoints mLabels

-- Write aggregates
epWriteCommunityAggregates iw aggregates

-- Use enrichedGraph'' for downstream export (has community_id set)
```

**File: `src/Graphos/Infrastructure/Export/Wiring.hs`**

Implement the new port method:
```haskell
epWriteCommunityAggregates = writeCommunityAggregates
```

**File: `src/Graphos/Infrastructure/Export/SQLite.hs`** (NEW)

Stream nodes, edges, and community-edge-pairs to `graph.sqlite`:
```haskell
module Graphos.Infrastructure.Export.SQLite
  ( writeSQLite
  ) where

import Database.SQLite3.Direct (DB, open, close, execute, executeMany, ...)

writeSQLite :: FilePath -> Graph -> CommunityMap -> [Edge] -> IO ()
writeSQLite path graph commMap edges = do
  db <- open path
  execute db "CREATE TABLE IF NOT EXISTS nodes (id TEXT PRIMARY KEY, label TEXT, source_file TEXT, file_type TEXT, kind TEXT, community_id INT, is_bridge INT, degree INT)"
  execute db "CREATE TABLE IF NOT EXISTS edges (id TEXT, src TEXT, tgt TEXT, relation TEXT, confidence REAL, src_community INT, tgt_community INT)"
  execute db "CREATE TABLE IF NOT EXISTS community_edge_pairs (src_cid INT, tgt_cid INT, count INT, PRIMARY KEY(src_cid, tgt_cid))"
  
  -- Batch insert nodes in transactions
  let nodeRows = ...  -- Map.elems (gNodes graph)
  executeMany db "INSERT INTO nodes VALUES (?, ?, ?, ?, ?, ?, ?, ?)" nodeRows
  
  -- Batch insert edges with denormalized community IDs
  let edgeRows = ...
  executeMany db "CREATE INDEX IF NOT EXISTS idx_edges_src ON edges(src_community)"
  executeMany db "CREATE INDEX IF NOT EXISTS idx_edges_tgt ON edges(tgt_community)"
  executeMany db "INSERT INTO edges VALUES (?, ?, ?, ?, ?, ?, ?)" edgeRows
  
  -- Insert community edge pairs
  let pairs = ...  -- from inter_community_edges of aggregates
  executeMany db "INSERT INTO community_edge_pairs VALUES (?, ?, ?)" pairs
  
  close db
```

**File: `src/Graphos/Server/Static.hs`**

Add COOP/COEP headers to every response:
```haskell
, responseHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Cross-Origin-Opener-Policy", "same-origin")
    , ("Cross-Origin-Embedder-Policy", "require-corp")
    ] ++ originalHeaders resp
```

**File: `.cabal`**

Add `direct-sqlite` to the `build-depends` if not already present.

### Key Decisions

- **Pipeline ordering**: join → write-aggregates → write-nodes → write-edges → write-analysis. This ensures aggregates are written before nodes (required for the streaming JSON structure).
- **SQLite batched inserts**: Use `executeMany` with transactions to keep peak memory flat. ~1K rows per transaction is the target batch size.
- **Edge denormalization**: Store `src_community` and `tgt_community` on every edge row so drill-down queries are single-table reads with an index — no JOIN needed per click.

### Dependencies

- Requires: Tasks 1, 2, 3 completed
- Reads: `tasks/04-wire-into-pipeline/plan.md`
- Unlocks: `tasks/04-wire-into-pipeline/check.md`
