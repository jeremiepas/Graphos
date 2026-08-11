# Tasks — Cluster Composition

## 1. CommunityComposition record + computation

### 1.1 Define the record
- [ ] Add `CommunityComposition` record in `src/Graphos/Domain/Community.hs` (or new `src/Graphos/Domain/Community/Composition.hs`): `ccCodeCount :: Int`, `ccDocCount :: Int`, `ccOtherCount :: Int`, `ccDominantKind :: Maybe Text`, `ccMixedRatio :: Double`, `ccCodeDocEdges :: Int`
- [ ] `ccDocCount` counts `DocFile` + `PaperFile` + `OfficeFile` together
- [ ] `ccOtherCount` counts `ImageFile` + `VideoFile` + `AudioFile`
- [ ] `ccMixedRatio = if max code doc == 0 then 0 else min code doc / max code doc`
- [ ] `ToJSON` / `FromJSON` instances (field names: `code`, `doc`, `other`, `dominant_kind`, `mixed_ratio`, `code_doc_edges`)
- [ ] Hspec: pure-code community → ratio 0; balanced → 1; paper counts as doc; round-trip JSON

### 1.2 Implement computeCompositions
- [ ] Add `computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition` in same module
- [ ] For each community: count members by `FileType`; derive `ccDominantKind` from most frequent non-Nothing `nodeKind`; count `References` edges inside community crossing `CodeFile`↔doc-like
- [ ] `ccCodeDocEdges` counts only `References` edges (not `contains` or others) with one endpoint `CodeFile` and the other doc-like
- [ ] Hspec: composition counts match membership; cross-type edge count excludes non-`References`; dominant kind ignores `Nothing`; 3 cross edges → `ccCodeDocEdges = 3`

## 2. Persist compositions to graph.json

### 2.1 Add field to Graph
- [ ] Add `gCompositions :: Maybe (Map CommunityId CommunityComposition)` to `Graph` in `src/Graphos/Domain/Graph/Core.hs` (additive; `Nothing` default)
- [ ] Update `Graph` `ToJSON` to write `compositions` field (omit when `Nothing`)
- [ ] Update `Graph` `FromJSON` to read `compositions` (default `Nothing` when absent)
- [ ] Hspec: `Graph` round-trips with and without `compositions`; legacy JSON loads as `Nothing`

### 2.2 Wire into pipeline
- [ ] In `src/Graphos/UseCase/Pipeline/Core.hs`: after Leiden produces `CommunityMap`, call `computeCompositions g commMap` and attach result to graph as `gCompositions = Just ...`
- [ ] The graph JSON output now includes `compositions` key
- [ ] Hspec (integration): pipeline run produces `graph.json` with `compositions` key

### 2.3 Load compositions in loadGraphFromFile
- [ ] In `src/Graphos/UseCase/Load.hs`: parse `compositions` from `graph.json`; `Nothing` on legacy
- [ ] Hspec: graph with `compositions` loads them; legacy graph without key loads `Nothing`

## 3. HTML viewer composition badge

### 3.1 Render badge in HTML
- [ ] In `src/Graphos/Infrastructure/Export/HTML.hs`: add a `compositionBadge(composition)` JS function returning `🔧 N / 📄 M / 🌉 K` string
- [ ] On community dots (overview mode): add badge to tooltip (vis-network `title` field)
- [ ] On community drill-down header: add badge as static text next to community label
- [ ] When `compositions` is absent in embedded JSON: omit badge (no error, no placeholder)
- [ ] Manual verification: `graphos serve` on a graph with compositions shows badges; legacy graph shows no badges

## 4. Composition-aware labeling prompt

### 4.1 Update labelPrompt
- [ ] In `src/Graphos/Domain/Labeling.hs` `labelPrompt`: accept an optional `Map CommunityId CommunityComposition` parameter (or read from `Graph` if `gCompositions` is wired)
- [ ] When compositions available for a community:
  - Tag each top node with `(code)` or `(doc)` based on `nodeFileType`
  - Split into `"Top code nodes:"` and `"Top doc nodes:"` lines (omit empty line)
  - Add composition line: `"composition: N code + M docs, K code↔doc links"`
  - Update preamble to "code-and-knowledge architecture analyst" + "name the CONCEPT that unifies"
- [ ] When compositions absent: fall back to today's flat list (single "Top nodes:" line, no tags, no composition line, existing preamble) — graceful degradation
- [ ] Hspec: mixed cluster prompt has both "Top code nodes:" and "Top doc nodes:"; pure-code has only "Top code nodes:"; preamble contains "concept" or "unifies"; legacy (no compositions) falls back to flat format

### 4.2 Wire compositions into labeling
- [ ] In `src/Graphos/UseCase/Label.hs` `labelCommunities` / `labelBatch`: pass `gCompositions` from the graph to `labelPrompt`
- [ ] If `gCompositions = Nothing`, `labelPrompt` falls back gracefully
- [ ] Hspec (integration): labeling on a graph with compositions produces the new prompt; labeling on a legacy graph produces today's prompt

## 5. Build + cross-cutting

### 5.1 Legacy graph compatibility
- [ ] Verify: `graph.json` without `compositions` loads; `gCompositions = Nothing`; all existing query-family commands work
- [ ] Verify: `labelCommunities` on a legacy graph produces today's prompt (no crash)
- [ ] Hspec: legacy graph fixture loads and queries without error

### 5.2 Build + warnings
- [ ] `cabal build` with `-Wall -Werror` clean
- [ ] `cabal test` green (existing tests + new Hspec cases)

### 5.3 Manual mixed-corpus verification
- [ ] Build a mixed corpus (this repo + `docs/`); confirm `graph.json` has `compositions` with non-zero `ccMixedRatio` on mixed communities
- [ ] Serve and confirm HTML viewer shows badges on community dots + drill-down headers
- [ ] Run labeling and confirm LLM labels name concepts ("Authentication") rather than code identifiers ("verifyToken") on mixed clusters
- [ ] Load a legacy `graph.json` (pre-this-change) and confirm no badge, no error, labeling falls back