# Design — Research View

## Context

This change adds a `graphos research` subcommand that runs N scored queries, takes the
union of their matched nodes, induces the subgraph, and renders an interactive HTML + JSON
artifact. Three design decisions matter: (1) how the multi-query union is computed and
attributed, (2) how the induced subgraph is extracted from the existing `Graph`/`GraphIndex`,
and (3) how the HTML rendering reuses `Infrastructure.Export.HTML` without duplicating it.

## Decision 1 — Multi-query union with per-term discovery attribution

### The contract today

`queryGraphWithIndexScored :: Graph -> GraphIndex -> Text -> ... -> QueryResponse` returns
`qrNodes :: [ScoredNode]` where each `ScoredNode` has `snNodeId`, `snScore`, `snLabel`,
`snSourceFile`, `snCommunity`. A single query is one call; there is no multi-query primitive.

### The problem

A "research view" needs to know **which query term(s) discovered each node** so the HTML
legend can color-code by discovery and the detail panel can show "found by: phase, work,
block spec". A naive union (`Set.union` of node ids) loses this attribution.

### Choice: `Map NodeId [Term]` alongside the union

```haskell
-- In Domain/Query/Research.hs
data ResearchNode = ResearchNode
  { rnNode      :: Node              -- full node from gNodes
  , rnDiscoveredBy :: [Text]         -- which terms matched it (ordered by input)
  , rnBestScore :: Double            -- highest score across discovering terms
  , rnScores    :: [(Text, Double)]  -- per-term scores (0 if a term didn't match)
  }

data ResearchView = ResearchView
  { rvTerms       :: [Text]                       -- input terms, in order
  , rvNodes       :: [ResearchNode]               -- union, deduplicated by NodeId
  , rvEdges       :: [Edge]                       -- induced: both endpoints in union
  , rvCommunities :: Map CommunityId ResearchCommunity
  , rvMetadata    :: ResearchMetadata
  }

data ResearchCommunity = ResearchCommunity
  { rcLabel       :: Maybe Text
  , rcComposition :: Maybe CommunityComposition   -- null when gCompositions absent
  , rcMemberCount :: Int
  }

data ResearchMetadata = ResearchMetadata
  { rmGeneratedAt :: UTCTime
  , rmGraphHash   :: Text          -- gHash from graph.json
  , rmNodeCount   :: Int
  , rmEdgeCount   :: Int
  }
```

The builder runs each query, collects `ScoredNode`s, and folds them into a
`Map NodeId ResearchNode` — accumulating `rnDiscoveredBy` and `rnScores` while keeping the
max score as `rnBestScore`. This preserves attribution without a separate join pass.

```haskell
-- In UseCase/Query/Research.hs
buildResearchView :: Graph -> GraphIndex -> Map CommunityId CommunityComposition
                  -> [Text] -> Maybe RefineConfig -> ResearchView
buildResearchView g idx comps terms mRefine =
  let perTermResults = map (\t -> (t, runQuery g idx t mRefine)) terms
      nodeMap        = foldl' (accumulateResults) Map.empty perTermResults
      unionIds       = Map.keysSet nodeMap
      inducedEdges   = filter (\e -> edgeSource e `Set.member` unionIds
                                  && edgeTarget e `Set.member` unionIds) (gEdges g)
      communities    = collectCommunities g idx comps unionIds
   in ResearchView { rvTerms = terms, rvNodes = Map.elems nodeMap
                   , rvEdges = inducedEdges, rvCommunities = communities
                   , rvMetadata = mkMeta g (Map.size nodeMap) (length inducedEdges) }
  where
    runQuery g idx t mRefine =
      let resp = queryGraphWithIndexScored g idx t defaultQueryBudget
       in maybe id (refineResponse) mRefine resp
    accumulateResults acc (term, resp) =
      foldl' (\acc' sn -> Map.insertWith (mergeNode term) (snNodeId sn)
                (mkResearchNode term sn) acc') acc (qrNodes resp)
```

### Single-term equivalence

When `terms = [t]`, `rvNodes` MUST have the same `NodeId` set as `graphos query t --json`.
This is a testable invariant (Hspec) and guards against union bugs.

## Decision 2 — Induced subgraph extraction

### The problem

The union gives us nodes; we need the **induced subgraph** — every edge in `gEdges` where
both `edgeSource` and `edgeTarget` are in the union set. This is a pure filter, but on a
10K-node graph with 100K edges, the filter must be efficient.

### Choice: `Set` membership filter, single pass

```haskell
inducedEdges :: Graph -> Set NodeId -> [Edge]
inducedEdges g unionIds =
  filter (\e -> edgeSource e `Set.member` unionIds
             && edgeTarget e `Set.member` unionIds) (gEdges g)
```

`Set.member` is O(log n); on a 100K-edge graph this is ~1.7M comparisons — well under the
500ms budget. No fancy indexing needed; the edge list is already materialized in `Graph`.

### Edge refinement inherits `--edges semantic|all`

The `--edges` flag (from `query-noise-control`) applies to the induced edges: in `semantic`
mode, trivia-target `contains` edges and self-edges are dropped before rendering. This is
the same `refineEdges` function the query family uses — no new logic.

## Decision 3 — HTML rendering: reuse `Infrastructure.Export.HTML`

### The contract today

`renderHtml :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> Analysis ->
  Text` produces `graph.html` — a self-contained file with vis-network, community coloring,
a navigator search box, and node hover/click. It's ~800 lines of HTML/JS scaffolding.

### The problem

A research view needs the **same** canvas, coloring, and hover behavior, plus two
additions: (a) a "discovered by" legend listing each term with its color, and (b) a detail
panel showing `rnDiscoveredBy`, `rnScores`, and `rnBestScore` on hover. Duplicating the
scaffolding would be ~800 lines of copy-paste.

### Choice: parameterize `renderHtml`, add a `renderResearchHtml` wrapper

```haskell
-- In Infrastructure/Export/HTML.hs

-- Existing function is refactored to accept optional "discovery" metadata
data HtmlRenderConfig = HtmlRenderConfig
  { hrcNodeColors    :: Maybe (NodeId -> HexColor)    -- override community coloring
  , hrcDiscoveryMeta :: Maybe (NodeId -> [Text])      -- "discovered by" for detail panel
  , hrcTitle         :: Text
  , hrcLegendItems   :: [(Text, HexColor)]            -- legend rows
  }

renderHtmlConfig :: Graph -> GraphIndex -> Map CommunityId CommunityComposition
                 -> Analysis -> HtmlRenderConfig -> Text

-- New wrapper
renderResearchHtml :: ResearchView -> Graph -> GraphIndex
                   -> Map CommunityId CommunityComposition -> Text
renderResearchHtml rv g idx comps =
  let termColors = assignTermColors (rvTerms rv)       -- one color per term
      nodeColor n = case rnDiscoveredBy <$> lookupNode n rv of
        Just (t:_)  -> Just (termColors ! t)            -- color by first-discovering term
        _           -> Nothing                          -- fall back to community color
      cfg = HtmlRenderConfig
        { hrcNodeColors = Just nodeColor
        , hrcDiscoveryMeta = Just (\n -> rnDiscoveredBy =<< lookupResearchNode n rv)
        , hrcTitle = "Research View — " <> T.intercalate ", " (rvTerms rv)
        , hrcLegendItems = map (\t -> (t, termColors ! t)) (rvTerms rv)
        }
   in renderHtmlConfig (inducedGraph rv) idx comps (emptyAnalysis) cfg
```

The `inducedGraph rv` builds a `Graph` containing only the union nodes + induced edges, so
vis-network renders just the subgraph (not the full graph with dimming). The detail panel is
a new `<div id="research-detail">` populated on node hover via the existing vis-network
`selectNode` event — a ~30-line JS addition to the existing scaffolding.

### Offline self-containedness

The HTML inlines vis-network from a CDN `<script>` tag today. For `file://` use, we have two
options: (a) keep the CDN tag (requires network on first open) or (b) inline the vis-network
JS. **Choice: keep the CDN tag** to match the existing `graph.html` behavior — the
`query-http-port` offline-fallback contract is about the navigator search falling back to
client-side substring, not about the library itself being inlined. If a fully-offline
research view is needed later, a `--inline-js` flag can be added (out of scope here).

## Decision 4 — `--subgraph <term>` for seed expansion

### The problem

A pure multi-query union can miss structurally important nodes that no query term matches
directly but that sit between matched nodes (e.g., a shared dependency). The user may want
to add "seed" nodes and expand around them.

### Choice: optional `--subgraph <term>...` adds seed nodes + 1-hop BFS

```haskell
-- --subgraph terms are resolved as query terms too, but their matched nodes
-- are added as seeds, and the induced subgraph is expanded by 1 BFS hop
-- (all neighbors of union nodes are added, then induce is re-run).
--
-- This is additive: --subgraph never removes nodes, only adds.
```

The `--subgraph` flag runs the same query path, adds the matched nodes to the union, then
expands the union by one BFS hop (`neighbors` of every union node). The induced subgraph is
then recomputed on the expanded union. This is a single `Set.union` + a `foldMap neighbors`
— O(union size × avg degree), well under budget.

If `explorer-queries`'s `around` has landed, `--subgraph` can reuse `aroundNode` for the
expansion. If not, it uses `bfsFrom` from `GraphIndex` directly. No hard dependency.

## What this design does NOT decide

- **In-navigator "Research" tab** — that's a `navigator-query-view` follow-up, not this
  change. This change produces a standalone HTML file.
- **Sessionful research (save/load research bundles)** — a future `--save`/`--load` flag
  could persist the `ResearchView` JSON and re-render later. Out of scope; the JSON output
  already enables external tooling to do this.
- **Automatic term suggestion** — "given one term, suggest related terms" is an LLM-powered
  feature, not a graph operation. Out of scope; the user supplies terms manually or via
  `--terms-file`.