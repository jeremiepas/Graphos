## Context

The cypher evaluator (`src/Graphos/Domain/Query/Cypher/Eval.hs`) currently ignores the `GraphIndex` (`_idx`) and full-scans `gNodes`/`gEdges` for every query. The design (see archived `opencypher-gql-query/design.md`) specifies: "Evaluate over `GraphIndex` + `CachedFGL`. Anchored patterns start from an index lookup; variable-length paths use the cached FGL (shared with analysis)."

The existing query system already provides the needed primitives:
- `GraphIndex.giLabelIndex :: Map Text [NodeId]` — label → node ID list (in `Graphos.Domain.Graph.Index`).
- `toCachedFGL :: Graph -> CachedFGL` — build the cached FGL graph once (in `Graphos.Domain.Graph.Analysis`).
- `cachedFindIdx :: CachedFGL -> NodeId -> Maybe Int` — NodeId → FGL index (in `Graphos.Domain.Graph.Analysis`).
- `breadthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId` (in `Graphos.Domain.Graph.Query`).
- `depthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId` (in `Graphos.Domain.Graph.Query`).
- `shortestPathWithCached :: CachedFGL -> NodeId -> NodeId -> Maybe [NodeId]` (in `Graphos.Domain.Graph.Query`).

See proposal.md for motivation.

## Goals / Non-Goals

**Goals:**
- Anchor node candidate lookups via `GraphIndex.giLabelIndex` (no full-scan of `gNodes`).
- Use `CachedFGL` for variable-length path enumeration (no recursive full-scan of `gEdges`).
- Build the `CachedFGL` once per query (not per hop).

**Non-Goals:**
- No behavior changes (query results must remain identical; all 558 tests must pass unchanged).
- No API changes, no dependency changes.
- No changes to the parser, AST, or rendering.

## Decisions

### Decision 1: Anchor node candidates via `giLabelIndex`

- **Choice**: In `nodeCandidates`, use `giLabelIndex` (via `Map.findWithDefault` on the lowercased label) to get candidate node IDs for a label filter, then filter by property constraints. When the pattern has no label constraint, fall back to the full node list.
- **Rationale**: `giLabelIndex :: Map Text [NodeId]` already maps labels to node ID lists, so a label filter is an O(1) map lookup instead of an O(n) scan.
- **Alternatives considered**:
  - Keep full-scanning `gNodes` (current) — simple but O(n) per query, doesn't scale.
  - Use `findBestNodeWithIndex` — designed for fuzzy term matching, not exact label filters; `giLabelIndex` is a better fit for exact label matches.

### Decision 2: Use `CachedFGL` for variable-length paths

- **Choice**: Build `CachedFGL` once per query via `toCachedFGL g`, then use the FGL graph (from `CachedFGL`) to enumerate variable-length paths. Precompute an edge adjacency index (source → edges, target → edges) from the FGL graph once per query, and use it in `hop` instead of full-scanning `Map.toList (gEdges g)`.
- **Rationale**: The FGL graph (from `CachedFGL`) is a Patricia-tree-based adjacency structure, so edge lookup is O(log n) instead of O(edges). Precomputing the adjacency index once per query avoids rebuilding it per hop.
- **Alternatives considered**:
  - Keep recursive full-scan (current) — simple but O(hops × edges × nodes), doesn't scale.
  - Use `breadthFirstSearchWithCached` / `depthFirstSearchWithCached` directly — these return `Set NodeId` (reachability), not edge-sequence paths; they can be used for reachability pruning but not for path reconstruction.
  - Use `shortestPathWithCached` — only for shortest path, not variable-length enumeration.

### Decision 3: Build `CachedFGL` once per query

- **Choice**: Call `toCachedFGL g` once in `evaluate` and pass the `CachedFGL` to all path-enumeration helpers.
- **Rationale**: Avoids rebuilding the FGL conversion per hop.

## Risks / Trade-offs

- [Risk] `giLabelIndex` may not be populated for all labels → Mitigation: Fall back to the full node list if the label is not in the index.
- [Risk] The FGL graph edge labels may not carry all edge properties → Mitigation: Use the original `Graph` for property lookups; use the FGL graph only for adjacency.
- [Risk] Behavior change (query results differ) → Mitigation: All existing tests must pass unchanged (558/0/3).
- [Risk] `CachedFGL` build cost (O(n + e)) may dominate for small graphs → Mitigation: Only build `CachedFGL` when the query has variable-length paths; skip it for single-hop queries.
