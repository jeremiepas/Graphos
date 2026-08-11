# Design — Explorer Queries

## Context

This change adds `around`/`cluster` subcommands and filter flags to the query family. Two
design decisions matter: how `around` fits the `query-cli-contract` JSON shape, and how
filter flags compose with the existing `refineResponse` pipeline.

## Decision 1 — `around` and `query-cli-contract`

### The contract today

`query-cli-contract` requires every query-family subcommand to:
- accept `--graph`, `--budget`, `--json`, `--label-width`, `--edges`
- emit a single JSON document on `--json` with `verdict`/`bestScore`/`hash`/`nodes`/`edges`/
  `suggestions` where applicable
- no interleaved log lines on stdout

### The problem

`around` is **not a search** — it doesn't have a `verdict` or `bestScore` (no query text to
score against). Forcing it into `QueryResponse` would mean populating `verdict = "none"` and
`bestScore = 0` — technically conformant but semantically wrong. Same for `cluster`.

### Choice: own response types, same flag surface

`around` and `cluster` get their own response types (`AroundResponse`, `ClusterResponse`),
but **honor the same flags** (`--json`, `--budget`, `--label-width`, `--edges`) and the same
"single JSON document, no interleaved logs" rule. The `query-cli-contract` spec is extended
to say: *"subcommands without a search query (`around`, `cluster`) emit their own response
type with `--json`, but honor the uniform flag surface and the single-document rule."*

```json
// graphos around mod_Auth --json
{
  "node": { "id": "mod_Auth", "label": "Auth", "file_type": "code", "kind": "module", ... },
  "in_edges":  [ { "edge": { "source": "mod_Config", "target": "mod_Auth", "type": "imports", "confidence": 0.9 },
                   "node": { "id": "mod_Config", "label": "Config", ... } }, ... ],
  "out_edges": [ { "edge": { "source": "mod_Auth", "target": "fn_verifyToken", "type": "calls", "confidence": 0.8 },
                   "node": { "id": "fn_verifyToken", "label": "verifyToken", ... } }, ... ],
  "community": { "id": 483, "label": "Authentication",
                 "composition": { "code": 12, "doc": 4, "mixed_ratio": 0.33, "code_doc_edges": 3 } },
  "bridges":    [ "mod_Auth", "fn_verifyToken" ],
  "depth":      1
}
```

`community.composition` is `null` when `gCompositions` is absent (legacy graph). `bridges`
is the subset of articulation points that are members of the node's community.

```json
// graphos cluster 483 --json
{
  "id": 483,
  "label": "Authentication",
  "composition": { "code": 12, "doc": 4, "other": 0, "dominant_kind": "function", "mixed_ratio": 0.33, "code_doc_edges": 3 },
  "members_by_kind": {
    "function": [ "fn_verifyToken", "fn_refreshToken", ... ],
    "section":  [ "sec_JWT_validation", "sec_Auth_flow", ... ],
    "module":   [ "mod_Auth" ]
  },
  "bridges":          [ "mod_Auth" ],
  "cross_type_edges": [ { "source": "sec_JWT_validation", "target": "fn_verifyToken", "type": "references", "confidence": 0.82 }, ... ]
}
```

`composition` is `null` when absent. `cross_type_edges` is always computable from the graph
edges (doesn't need `gCompositions` — it's an edge filter).

## Decision 2 — Filter flags as post-filters in `refineResponse`

The new flags (`--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only`) are
**post-filters** on the result set — they don't change the query algorithm, only narrow
what is returned. This keeps `queryGraphWithIndexScored` unchanged; the filter applies in
`refineResponse` (already the place where `EdgeMode` and label-width truncation happen).

```haskell
-- In UseCase/Query/Refine.hs
data ExplorerFilter = ExplorerFilter
  { efFiletype   :: Maybe FileType
  , efKind       :: Maybe Text
  , efMixedOnly  :: Bool
  }

applyExplorerFilter :: ExplorerFilter
                   -> GraphIndex
                   -> Map CommunityId CommunityComposition  -- empty when absent
                   -> QueryResponse
                   -> QueryResponse
applyExplorerFilter ef idx comps resp =
  resp { qrNodes = filterNodes ef idx comps (qrNodes resp)
       , qrEdges = filterEdges ef (qrEdges resp)  -- edges touching dropped nodes are dropped
       }
  where
    filterNodes ef idx comps = filter keep
      where
        keep sn = filetypeOk && kindOk && mixedOk
          where
            n = ... lookup node by snNodeId ...
            filetypeOk = case efFiletype ef of
                          Nothing -> True
                          Just ft -> nodeFileType n == ft
            kindOk = case efKind ef of
                      Nothing -> True
                      Just k -> nodeKind n == Just k
            mixedOk = if not (efMixedOnly ef)
                      then True
                      else case communityOfNode (snNodeId sn) idx >>= flip Map.lookup comps of
                             Just comp -> ccMixedRatio comp > 0
                             Nothing   -> True  -- no community assigned → keep (don't drop on missing data)
    filterEdges ef = filter edgeKeepsBothEndpoints
```

### `--mixed-only` graceful degradation

When `compositions` is empty (legacy graph, `gCompositions = Nothing`):
- `--mixed-only` cannot filter (no `ccMixedRatio` data)
- Log a warning: `"--mixed-only ignored: no community compositions available (legacy graph)"`
- Return all nodes (no-op)

This is honest: the flag does nothing without the data, and the user is told why.

### `--code-only` / `--doc-only` shorthand

`--code-only` = `--filetype code`; `--doc-only` = `--filetype doc`. If both `--code-only` and
`--filetype paper` are passed, that's a conflict — error clearly. Otherwise, the shorthand
sets `efFiletype`.

## Decision 3 — `aroundNode` orchestration

```haskell
aroundNode :: Graph
          -> GraphIndex
          -> Map CommunityId CommunityComposition  -- empty when absent
          -> NodeId
          -> Int               -- depth (default 1)
          -> Maybe ExplorerFilter
          -> Either Text AroundResponse
aroundNode g idx comps nodeId depth mFilter =
  case Map.lookup nodeId (gNodes g) of
    Nothing -> Left $ "Node not found: " <> nodeId
    Just node ->
      let inEdges  = [(e, neighborNode e) | e <- edgesTo g nodeId]    -- edges where target == nodeId
          outEdges = [(e, neighborNode e) | e <- edgesFrom g nodeId]  -- edges where source == nodeId
          community = case communityOfNode nodeId idx of
            Nothing -> Nothing
            Just cid -> Just (cid, communityLabel idx cid, Map.lookup cid comps)
          bridges = case community of
            Nothing -> []
            Just (cid, _, _) -> filter (`memberOf` cid) (articulationPoints g)
          resp = AroundResponse node inEdges outEdges community bridges depth
      in Right (maybe id (applyExplorerFilterAround ef) mFilter resp)
```

Reuses: `gNodes` lookup, `edgesTo`/`edgesFrom` (or `neighbors` + edge direction),
`communityOfNode`, `articulationPoints`. All exist today. The only new logic is the
composition into one response + the filter application.

### `resolveNodeArg` reuse

`around <node>` accepts either a `NodeId` (`mod_Auth`) or a display label (`Auth`) — reuses
the same `resolveNodeArg` helper that `fix-query-cli-ergonomics` adds to `neighbors`. If
that change hasn't landed yet, `around` implements its own label fallback (or waits for
that change — dependency noted in tasks).

## Decision 4 — Uniform acceptance: no-op vs. error

The `query-cli-contract` uniform-acceptance rule says every flag is accepted by every
subcommand. For the new filter flags:

| Flag | `query` | `path` | `explain` | `symbols` | `neighbors` | `around` | `cluster` |
|------|---------|--------|-----------|-----------|-------------|----------|-----------|
| `--filetype` | filter | filter path nodes | filter | filter | filter | filter edges | filter members |
| `--kind` | filter | filter path nodes | filter | filter | filter | filter edges | filter members |
| `--mixed-only` | filter | **no-op** | **no-op** | filter | filter | filter edges | **no-op** (cluster is already chosen) |
| `--code-only` | = `--filetype code` | same | same | same | same | same | same |
| `--doc-only` | = `--filetype doc` | same | same | same | same | same | same |

`--mixed-only` on `path`/`explain`/`cluster` is a no-op (accepted, does nothing). This is
honest — `path` doesn't return a community-scoped result set to filter. The alternative
(erroring on inapplicable flags) violates uniform acceptance.

## What this design does NOT decide

- **HTTP endpoint shape** — inherits from `add-query-api-port-and-view`; this change adds
  `/api/around` and `/api/cluster` once that change lands. Task 3.7 is explicitly deferred.
- **`graphos-query` skill update** — once `around` lands, it should become the recommended
  first call for agents. That's a skill-doc change, not a code change; happens after
  implementation, not part of this change.
- **Sessionful exploration (frontier-carrying)** — that's a separate, larger capability
  (stateful MCP sessions with expand/pivot/contract). This change is stateless; sessionful
  exploration is a follow-up if `around` proves insufficient.