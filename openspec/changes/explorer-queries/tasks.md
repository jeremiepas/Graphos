# Tasks — Explorer Queries

## 1. ExplorerFilter + applyExplorerFilter

### 1.1 Define ExplorerFilter
- [ ] Add `ExplorerFilter` record in `src/Graphos/UseCase/Query/Refine.hs`: `efFiletype :: Maybe FileType`, `efKind :: Maybe Text`, `efMixedOnly :: Bool`
- [ ] Add `emptyExplorerFilter :: ExplorerFilter` (all `Nothing`/`False`)
- [ ] Hspec: record round-trips; empty filter is identity

### 1.2 Implement applyExplorerFilter
- [ ] Implement `applyExplorerFilter :: ExplorerFilter -> GraphIndex -> Map CommunityId CommunityComposition -> QueryResponse -> QueryResponse`
- [ ] `efFiletype`: drop nodes whose `nodeFileType` doesn't match
- [ ] `efKind`: drop nodes whose `nodeKind` doesn't match (keep nodes with `nodeKind = Nothing`? — decide: drop, since `--kind function` should only return functions)
- [ ] `efMixedOnly`: drop nodes whose community has `ccMixedRatio == 0`; keep nodes with no community assigned (don't drop on missing data)
- [ ] Drop edges that touch dropped nodes (so the subgraph stays consistent)
- [ ] When `compositions` map is empty AND `efMixedOnly`: log warning, no-op (return all)
- [ ] Hspec: filetype filter narrows; kind filter narrows; mixed-only drops pure communities; mixed-only no-op on empty compositions warns; no filter = identity; edges touching dropped nodes are removed

## 2. aroundNode orchestration

### 2.1 Define AroundResponse
- [ ] Add `AroundResponse` record (in `src/Graphos/Domain/Graph/Score.hs` or new `src/Graphos/Domain/Query/Explorer.hs`): `arNode :: Node`, `arInEdges :: [(Edge, Node)]`, `arOutEdges :: [(Edge, Node)]`, `arCommunity :: Maybe (CommunityId, Maybe Text, Maybe CommunityComposition)`, `arBridges :: [NodeId]`, `arDepth :: Int`
- [ ] `ToJSON` instance with field names: `node`, `in_edges`, `out_edges`, `community`, `bridges`, `depth`
- [ ] `community` serializes as `{ id, label, composition }` or `null` when no community
- [ ] Hspec: JSON shape matches design; `null` community when unassigned

### 2.2 Implement aroundNode
- [ ] Add `aroundNode :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> NodeId -> Int -> Maybe ExplorerFilter -> Either Text AroundResponse` in `src/Graphos/UseCase/Query.hs`
- [ ] Resolve node arg via `resolveNodeArg` (id-first, label fallback) — reuse from `fix-query-cli-ergonomics` if landed, else implement inline
- [ ] Compute in-edges: edges where `edgeTarget == nodeId`, with the source neighbor node
- [ ] Compute out-edges: edges where `edgeSource == nodeId`, with the target neighbor node
- [ ] At `--depth N > 1`: BFS expand in/out edges N hops (reuse `bfsFrom` from `GraphIndex`)
- [ ] Look up community via `communityOfNode nodeId idx`; attach `(cid, label, composition)` if present
- [ ] Get community bridges: `articulationPoints g` filtered to members of the node's community
- [ ] Apply `ExplorerFilter` to `arInEdges`/`arOutEdges` (drop edges whose neighbor doesn't match filter)
- [ ] Return `Left "Node not found: ..."` on unknown node
- [ ] Hspec: around returns expected fields; unknown node → `Left`; label resolution works; `--depth 2` includes 2-hop neighbors; filter narrows edges; community is `Nothing` when unassigned

## 3. clusterDetail orchestration

### 3.1 Define ClusterResponse
- [ ] Add `ClusterResponse` record: `crId :: CommunityId`, `crLabel :: Maybe Text`, `crComposition :: Maybe CommunityComposition`, `crMembersByKind :: Map Text [NodeId]`, `crBridges :: [NodeId]`, `crCrossTypeEdges :: [Edge]`
- [ ] `ToJSON` instance with field names: `id`, `label`, `composition`, `members_by_kind`, `bridges`, `cross_type_edges`
- [ ] `composition` is `null` when `gCompositions` absent
- [ ] Hspec: JSON shape matches design; `null` composition on legacy graph

### 3.2 Implement clusterDetail
- [ ] Add `clusterDetail :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> CommunityId -> Maybe ExplorerFilter -> Either Text ClusterResponse` in `src/Graphos/UseCase/Query.hs`
- [ ] Look up community in `CommunityMap` (via `GraphIndex` or passed separately); return `Left "Community not found: ..."` if absent
- [ ] Group members by `nodeKind` (ignore `Nothing` kinds — put in a `"unknown"` bucket or omit? — decide: omit, since `--kind` filter on `cluster` should narrow)
- [ ] Collect `cross_type_edges`: `References` edges inside the community with one endpoint `CodeFile` and the other doc-like
- [ ] Get community bridges: `articulationPoints g` filtered to community members
- [ ] Attach `crComposition` from `Map.lookup cid comps` (or `Nothing`)
- [ ] Attach `crLabel` from community label map (via `GraphIndex` or passed)
- [ ] Apply `ExplorerFilter` to `crMembersByKind` (drop members not matching filetype/kind)
- [ ] Hspec: cluster returns composition + members grouped + cross-type edges; unknown community → `Left`; filter narrows members; `crComposition = Nothing` on legacy graph

## 4. CLI parser: filter flags + new subcommands

### 4.1 Add filter flags to CommonQueryOpts
- [ ] In `src/Graphos/CLI/Parser.hs`: add `--filetype <code|doc|paper|image|video|audio|office>`, `--kind <text>`, `--mixed-only` switch, `--code-only` switch, `--doc-only` switch to `CommonQueryOpts` (or a new `ExplorerFilterOpts` mixed in)
- [ ] `--code-only` sets `efFiletype = Just CodeFile`; `--doc-only` sets `efFiletype = Just DocFile`
- [ ] Conflict check: if both `--code-only` and `--filetype paper` are passed, error clearly
- [ ] Parse `--filetype` value into `FileType` (reuse existing `Read` instance or add a parser)
- [ ] Hspec: parser accepts all flags; `--help` lists them; invalid `--filetype` value errors; `--code-only` + `--filetype paper` conflicts

### 4.2 Add around and cluster subcommands
- [ ] Add `aroundOpts` with `<node>` positional arg + `--depth N` (default 1) + common flags + explorer filter flags
- [ ] Add `clusterOpts` with `<id>` positional arg (parse as `Int`) + common flags + explorer filter flags
- [ ] Register both as `command` entries in the query subcommand parser
- [ ] Hspec: parser accepts `around`/`cluster` with all flags; `--help` lists them; invalid `--depth` (0, negative) errors

## 5. app/Main.hs dispatch + rendering

### 5.1 Dispatch new subcommands
- [ ] In `app/Main.hs`: add cases for `Around cmd` and `Cluster cmd` in the command dispatch
- [ ] Call `aroundNode` / `clusterDetail` with the loaded graph, index, compositions, and filter
- [ ] Route to JSON rendering (if `--json`) or text rendering
- [ ] Hspec (integration): `graphos around mod_X --json` against `graphos-out/graph.json` returns valid JSON; `graphos cluster 0 --json` returns valid JSON

### 5.2 Text rendering for around and cluster
- [ ] Implement `renderAroundText :: AroundResponse -> Text` (human-readable summary: node + in/out edge counts + community + bridges)
- [ ] Implement `renderClusterText :: ClusterResponse -> Text` (community + composition + members by kind + bridges + cross-type edges)
- [ ] Hspec: text rendering shows same counts as JSON

### 5.3 Wire filter flags into existing query family
- [ ] In `app/Main.hs` dispatch for `query`/`symbols`/`neighbors`: parse `ExplorerFilter` from flags and pass to `applyExplorerFilter` in the `refineResponse` step
- [ ] Log warning when `--mixed-only` is set but compositions are empty
- [ ] Hspec: `graphos query "x" --filetype doc --json` narrows results; `--mixed-only` on legacy graph warns + no-op

## 6. HTTP port endpoints (deferred)

### 6.1 Add /api/around and /api/cluster
- [ ] **Dependency**: waits for `add-query-api-port-and-view` to merge
- [ ] In `src/Graphos/Infrastructure/Server/QueryAPI.hs`: add `GET /api/around?node=<id>&depth=<n>&filetype=...&kind=...&mixed-only=1` and `GET /api/cluster?id=<n>&filetype=...&kind=...`
- [ ] Reuse `aroundNode` / `clusterDetail` orchestration; ensure byte-for-byte parity with CLI `--json`
- [ ] Hspec: HTTP response equals CLI `--json` for same inputs

## 7. Build + cross-cutting

### 7.1 Legacy graph compatibility
- [ ] Verify: `graph.json` without `compositions` loads; `around`/`cluster` work (composition = `null`); `--mixed-only` warns + no-op
- [ ] Hspec: legacy graph fixture + `around`/`cluster`/`query --mixed-only` all work without error

### 7.2 Build + warnings
- [ ] `cabal build` with `-Wall -Werror` clean
- [ ] `cabal test` green (existing tests + new Hspec cases)

### 7.3 Manual mixed-corpus verification
- [ ] Build a mixed corpus; confirm `graphos around mod_Graphos --json` returns < 500ms on a 10K-node graph
- [ ] Confirm `graphos query "auth" --filetype doc --json` returns only `DocFile` nodes
- [ ] Confirm `graphos query "auth" --mixed-only --json` drops pure-community nodes (when compositions available)
- [ ] Confirm `graphos cluster 483 --json` shows members grouped by kind + cross-type edges
- [ ] Confirm legacy graph: `--mixed-only` warns + no-op; `around`/`cluster` work with `composition: null`