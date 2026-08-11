# Tasks — Research View

## 1. Domain types: ResearchView + ToJSON

### 1.1 Define ResearchView records
- [ ] Add `src/Graphos/Domain/Query/Research.hs` with `ResearchView`, `ResearchNode`, `ResearchCommunity`, `ResearchMetadata` records per design Decision 1
- [ ] Add `ToJSON` instances with field names: `terms`, `nodes` (each with `id`, `label`, `source_file`, `community`, `discovered_by`, `best_score`, `scores`), `edges` (each with `source`, `target`, `type`, `confidence`), `communities` (keyed by id, each with `label`, `composition`, `member_count`), `metadata` (`generated_at`, `graph_hash`, `node_count`, `edge_count`)
- [ ] `composition` serializes as `null` when `gCompositions` absent (legacy graph)
- [ ] Hspec: JSON shape matches design; `null` composition on legacy graph; round-trips via Aeson

### 1.2 Term color assignment
- [ ] Add `assignTermColors :: [Text] -> Map Text HexColor` — deterministic palette (e.g., D3 schemeCategory10) keyed by term
- [ ] Hspec: same terms → same colors; distinct terms → distinct colors; > 10 terms cycles palette

## 2. UseCase: buildResearchView (multi-query union + induce)

### 2.1 Implement buildResearchView
- [ ] Add `src/Graphos/UseCase/Query/Research.hs` with `buildResearchView :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> [Text] -> Maybe RefineConfig -> ResearchView`
- [ ] Run `queryGraphWithIndexScored` per term with the `--budget` default (2000); thread `RefineConfig` for `--edges` + noise control
- [ ] Fold results into `Map NodeId ResearchNode` — accumulate `rnDiscoveredBy` (ordered by input term order), `rnScores` (per-term), keep max as `rnBestScore`
- [ ] Compute induced edges: `filter (\e -> edgeSource e ∈ unionIds && edgeTarget e ∈ unionIds) (gEdges g)`; apply `refineEdges` for `--edges semantic|all`
- [ ] Collect communities: for each union node, look up its `CommunityId` via `communityOfNode`; group; attach label + composition (or `Nothing`)
- [ ] Build `ResearchMetadata` with `UTCTime` (via `getCurrentTime` in IO wrapper, not the pure function), `gHash`, counts
- [ ] Hspec: union equals set-union of per-term `qrNodes` ids; single-term equivalence with `query --json` node set; induced edges have both endpoints in union; `discovered_by` attribution is correct (a node matched by two terms has both in the list); empty results (all terms return no matches) → empty nodes + edges, non-empty metadata; `--edges semantic` drops trivia-target edges from induced set

### 2.2 Optional --subgraph seed expansion
- [ ] Add `expandWithSeeds :: Graph -> GraphIndex -> Set NodeId -> [Text] -> Set NodeId` — runs queries for seed terms, adds matched nodes to union, then 1-hop BFS expansion (`bfsFrom` or `neighbors`)
- [ ] Integration in `buildResearchView`: when `--subgraph` terms supplied, expand union before inducing edges
- [ ] Hspec: `--subgraph` adds nodes never reduces; expanded union includes all 1-hop neighbors of original union; induced edges recomputed on expanded set

## 3. HTML rendering: renderResearchHtml

### 3.1 Refactor renderHtml to accept HtmlRenderConfig
- [ ] In `src/Graphos/Infrastructure/Export/HTML.hs`: extract the existing `renderHtml` into `renderHtmlConfig :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> Analysis -> HtmlRenderConfig -> Text`
- [ ] Add `HtmlRenderConfig` record: `hrcNodeColors :: Maybe (NodeId -> HexColor)`, `hrcDiscoveryMeta :: Maybe (NodeId -> [Text])`, `hrcTitle :: Text`, `hrcLegendItems :: [(Text, HexColor)]`
- [ ] Keep existing `renderHtml` as a thin wrapper calling `renderHtmlConfig` with default config (no behavior change for `graph.html`)
- [ ] Hspec: existing `graph.html` rendering unchanged (regression); `renderHtmlConfig` with default config == old `renderHtml` output

### 3.2 Implement renderResearchHtml
- [ ] Add `renderResearchHtml :: ResearchView -> Graph -> GraphIndex -> Map CommunityId CommunityComposition -> Text`
- [ ] Build `inducedGraph :: ResearchView -> Graph` — a `Graph` containing only `rvNodes` (as `gNodes` entries) + `rvEdges` (as `gEdges`)
- [ ] Build `HtmlRenderConfig` with: `hrcNodeColors = Just (colorByFirstDiscoveringTerm rv)`, `hrcDiscoveryMeta = Just (\n -> rnDiscoveredBy <$> lookupResearchNode n rv)`, `hrcTitle = "Research View — " <> intercalate ", " (rvTerms rv)`, `hrcLegendItems = map (\t -> (t, termColors ! t)) (rvTerms rv)`
- [ ] Call `renderHtmlConfig inducedGraph idx comps emptyAnalysis cfg`
- [ ] Hspec: output is self-contained HTML (single `<html>` block); contains the term legend with one entry per term; contains `<div id="research-detail">` for the detail panel; node hover JS populates detail panel with `discovered_by` and `best_score`; no network calls required for static rendering (vis-network CDN tag is present, matching existing `graph.html`)

### 3.3 Detail panel JS
- [ ] Add `~30 lines` of JS to the HTML scaffolding: on vis-network `selectNode` event, populate `<div id="research-detail">` with the node's `discovered_by`, `scores`, `best_score`, `source_file`, `community`
- [ ] The JS reads from a JSON blob embedded in the HTML (`<script type="application/json" id="research-data">`) keyed by `NodeId`
- [ ] Hspec: HTML contains the embedded JSON blob; blob parses and contains all `rvNodes` keyed by id

## 4. CLI parser: research subcommand

### 4.1 Add research subcommand + flags
- [ ] In `src/Graphos/CLI/Parser.hs`: add `researchOpts` with positional `<term>...` (one or more), `--subgraph <term>...` (zero or more), `--terms-file <path>` (optional), `--label <text>` (optional), `--html` (switch, default on), `--json` (switch), common flags (`--graph`, `--budget`, `--label-width`, `--edges`)
- [ ] `--terms-file` reads newline-delimited terms; terms are appended to positional terms (dedup, preserve order)
- [ ] `--label` titles the output HTML and is used in the output filename; defaults to a timestamp (`research-YYYYMMDD-HHMMSS`)
- [ ] Register `research` as a new top-level command (sibling to `query`, `serve`, etc.)
- [ ] Hspec: parser accepts `research phase work block`; `--help` lists all flags; `--terms-file` with nonexistent path errors clearly; `--subgraph` with no positional terms errors (at least one term required); invalid `--edges` value errors

## 5. app/Main.hs dispatch + output

### 5.1 Dispatch research command
- [ ] In `app/Main.hs`: add case for `Research cmd`; load `graph.json`, build `GraphIndex`, load compositions
- [ ] Call `buildResearchView` with terms, seeds, `RefineConfig` from `--edges`/`--budget`
- [ ] If `--json`: emit `ResearchView` as a single JSON document on stdout (no interleaved logs)
- [ ] If `--html` (default): write `renderResearchHtml` to `graphos-out/research-<label>.html`
- [ ] If both `--json` and `--html`: write HTML to file and JSON to stdout (or to `graphos-out/research-<label>.json` if `--json-file` is passed — decide: `--json` to stdout, HTML to file, both is fine)
- [ ] Hspec (integration): `graphos research phase work --json` against `graphos-out/graph.json` returns valid `ResearchView` JSON on stdout; `graphos research phase work --html --label test` writes `graphos-out/research-test.html` that opens in a browser

### 5.2 Output filename + path
- [ ] Default output dir: `graphos-out/` (respect `--graph` parent dir if graph is elsewhere)
- [ ] Default filename: `research-<label-or-timestamp>.html` and `.json`
- [ ] `--output <path>` flag overrides the full path (optional, add if low-cost)
- [ ] Hspec: default filename contains the label or a timestamp; `--output` overrides

## 6. HTTP port endpoint (deferred)

### 6.1 Add /api/research
- [ ] **Dependency**: waits for `query-http-port` to merge
- [ ] In `src/Graphos/Infrastructure/Server/QueryAPI.hs`: add `GET /api/research?terms=a,b,c&subgraph=d,e&edges=semantic` returning the same `ResearchView` JSON as CLI `--json`
- [ ] Reuse `buildResearchView`; ensure byte-for-byte parity with CLI `--json`
- [ ] Hspec: HTTP response equals CLI `--json` for same inputs; `terms` parameter parsed comma-separated; empty `terms` returns 400

## 7. Build + cross-cutting

### 7.1 Legacy graph compatibility
- [ ] Verify: `graph.json` without `compositions` loads; `research` works (communities have `composition: null`); no crash
- [ ] Hspec: legacy graph fixture + `research` produces valid `ResearchView` with `composition: null` in communities

### 7.2 Build + warnings
- [ ] `cabal build` with `-Wall -Werror` clean
- [ ] `cabal test` green (existing tests + new Hspec cases)

### 7.3 Manual end-to-end verification
- [ ] Run `graphos research phase work block governance --html --label solario-phases` against the Solario Core `graph.json` (or this repo's `graph.json`)
- [ ] Open the generated HTML via `file://` in a browser; confirm: legend lists all 4 terms with distinct colors; nodes are color-coded by discovering term; hovering a node shows detail panel with `discovered_by`, `scores`, `best_score`; edges only connect union nodes
- [ ] Confirm `graphos research phase --json` node set == `graphos query phase --json` node set (single-term equivalence)
- [ ] Time end-to-end on a 10K-node graph: target < 2s (4 queries × < 500ms + induce + render)
- [ ] Confirm `--terms-file` with a 10-line file produces a research view with all 10 terms attributed