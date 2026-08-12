<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — HTML rendering: renderResearchHtml — PLAN

**Task slug**: `03-html-rendering-renderresearchhtml`
**Attempt**: 1
**Status**: pending

## Summary

Refactor the existing `renderHtml` to accept `HtmlRenderConfig` as a parameter, then implement `renderResearchHtml` as a wrapper that builds an induced subgraph from `ResearchView`, configures term-based coloring with discovery legend, and embeds a JSON blob + ~30 lines of JS for the detail panel.

## Detail

### Scope

- **Extend**: `src/Graphos/Infrastructure/Export/HTML.hs`
  - Extract existing `renderHtml` into `renderHtmlConfig :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> Analysis -> HtmlRenderConfig -> Text`
  - Add `HtmlRenderConfig` record: `hrcNodeColors :: Maybe (NodeId -> HexColor)`, `hrcDiscoveryMeta :: Maybe (NodeId -> [Text])`, `hrcTitle :: Text`, `hrcLegendItems :: [(Text, HexColor)]`
  - Keep existing `renderHtml` as a thin wrapper calling `renderHtmlConfig` with default config (no behavior change for `graph.html`)
- **New function**: `renderResearchHtml :: ResearchView -> Graph -> GraphIndex -> Map CommunityId CommunityComposition -> Text`
  - Builds `inducedGraph :: ResearchView -> Graph` — a `Graph` containing only `rvNodes` (as `gNodes` entries) + `rvEdges` (as `gEdges`)
  - Builds `HtmlRenderConfig` with: `hrcNodeColors = Just (colorByFirstDiscoveringTerm rv)`, `hrcDiscoveryMeta = Just (\n -> rnDiscoveredBy <$> lookupResearchNode n rv)`, `hrcTitle = "Research View — " <> intercalate ", " (rvTerms rv)`, `hrcLegendItems = map (\t -> (t, termColors ! t)) (rvTerms rv)`
  - Embeds a JSON blob `<script type="application/json" id="research-data">` keyed by `NodeId` with all node data
  - Calls `renderHtmlConfig inducedGraph idx comps emptyAnalysis cfg`
- **Detail panel JS**: ~30 lines of JS in the HTML scaffolding:
  - On vis-network `selectNode` event, populate `<div id="research-detail">` with the node's `discovered_by`, `scores`, `best_score`, `source_file`, `community`
  - Reads from the embedded JSON blob, not from network calls
- **Hspec module**: `test/Graphos/Infrastructure/Export/HTMLSpec.hs` (new test cases)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `test/Graphos/Infrastructure/Export/HTMLSpec.hs` (new + regression)
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: research produces self-contained HTML` (spec § "research produces self-contained HTML")
- `Scenario: legend lists all terms` (spec § "legend lists all terms")
- `Scenario: node colored by first discoverer` (spec § "node colored by first discoverer")
- `Scenario: detail panel populates on hover` (spec § "detail panel populates on hover")
- `Scenario: HTML opens offline` (spec § "HTML opens offline")

**PASS conditions**:
- Existing `graph.html` rendering is unchanged (regression test: `renderHtml` with default config produces identical output to previous `renderHtml`)
- `renderHtmlConfig` with default config == old `renderHtml` output (byte-identical for the same inputs)
- Output is self-contained HTML (single `<html>` block, no external dependencies beyond vis-network CDN script tag)
- HTML contains the term legend with one entry per term (distinct color swatches)
- HTML contains `<div id="research-detail">` for the detail panel
- HTML contains `<script type="application/json" id="research-data">` blob with all `rvNodes` keyed by id
- Blob parses and contains all `rvNodes` keyed by id
- Node hover JS populates detail panel with `discovered_by` and `best_score`
- No network calls required for static rendering (vis-network CDN tag present, matching existing `graph.html`)
- Existing `renderHtml` signature and behavior unchanged (backward compatible)

**FAIL boundaries**:
- If `renderHtml` output changes for the same inputs (regression), the test fails
- If the JSON blob is missing, malformed, or missing any node, the test fails
- If the legend does not contain all terms, the test fails
- If the HTML contains references to server endpoints (not self-contained), the test fails
- If `renderHtmlConfig` with default config differs from old `renderHtml` output, the test fails

### Affected modules

- **Extended**: `src/Graphos/Infrastructure/Export/HTML.hs`
- **New tests**: `test/Graphos/Infrastructure/Export/HTMLSpec.hs` (append new test cases for research HTML)
- **Imports from**: `src/Graphos/Domain/Query/Research.hs` (ResearchView, ResearchNode), `src/Graphos/Domain/Graph/Core.hs` (Graph, NodeId), `src/Graphos/Domain/Community.hs` (CommunityId, CommunityComposition), `src/Graphos/Domain/Graph/Analysis.hs` (Analysis, emptyAnalysis)

### Prerequisites

- Task 1 (Domain types) must be implemented first
- Task 2 (UseCase: buildResearchView) should be implemented first (induced graph concept)
- Existing `Infrastructure.Export.HTML` must exist with a `renderHtml` function (~800 lines)
- Existing vis-network scaffolding must be understood (the ~30-line JS addition pattern)

### Risks

- **High**: The existing `renderHtml` is ~800 lines of HTML/JS scaffolding; refactoring to accept `HtmlRenderConfig` while preserving exact output requires careful extraction without changing the embedded HTML/JS strings
- **Medium**: The detail panel JS (~30 lines) must integrate with the existing vis-network `selectNode` event handling without conflicts
- **Medium**: `inducedGraph` must correctly construct a `Graph` from `ResearchView` nodes + edges; if `NodeId` vs FGL `nid` conversion is needed, it adds complexity
- **Low**: The JSON blob embedding is straightforward (Aeson `encode` into a `<script>` tag)
