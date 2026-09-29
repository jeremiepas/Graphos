# Bounded report/export stage (single-pass analysis, no recomputation)

## Why

Measured 2026-09-29 on the Graphos repo root (`graphos . --no-viz`, 1243 files
→ 16,802 nodes, 19k extracted edges, 392,304 inferred edges, 893 communities):
Steps 1–5 completed in ~21 s, but the report/export stage (Steps 6–7) ran for
20+ minutes at up to **25.7 GB RSS** (~24 CPU-minutes, GC-bound on a ~26 GB
heap) before being killed. This is the same footprint shape as the 2026-09-22
kernel OOM. The new memory-budget-guard turns that into a controlled exit 2 —
but the stage should not need tens of GB to render a markdown file.

The cost is recomputation, not rendering. On the enriched (post-inference)
graph, Step 6/7 today:

- `generateReport` calls `articulationPoints g` **and** `biconnectedComponents g`,
  each performing its own `toCachedFGL` conversion of the 410k-edge graph —
  although `clusterGraph` already computed articulation points for community
  aggregates, and `articulationPointsWithCached`/`biconnectedComponentsWithCached`
  exist precisely to share one conversion. Neo4j/Memgraph subgraph pushes call
  `articulationPoints` yet again.
- `communitiesSection` recomputes `cohesionScore g members` for every
  community, and `lowCohesionQuestions` recomputes it a third time — although
  `analysisCohesion` already carries the `scoreAllCohesion` results from
  clustering.
- `analysisSurprises`/`analysisQuestions` are lazy thunks forced only at
  render time: full O(E) candidate lists over 410k edges are built and sorted
  inside the *export* stage, so 20 minutes of "export" is actually unattributed
  analysis (and the heap-guard's stage naming is misleading).
- `biconnectedComponents` materializes every component list when the report
  prints only the count.

## What Changes

- **Single-pass shared connectivity analysis** — one `toCachedFGL` per run on
  the enriched graph; articulation points and the biconnected-component count
  are computed once (`…WithCached`) in the cluster/analyze stage and carried
  on the `Analysis` record. `generateReport`, community aggregates, and the
  Neo4j/Memgraph subgraph pushes consume the shared values instead of
  recomputing.
- **Cohesion reuse** — the report's Communities table and the low-cohesion
  questions read `analysisCohesion`; `cohesionScore` is never recomputed
  during report rendering.
- **Bounded top-N selection** — surprising connections and suggested
  questions select their top-N with an O(E · log N) bounded accumulator
  instead of materializing and sorting the full candidate list.
- **Count-only biconnected components** — a `biconnectedComponentCount`
  (cached-FGL variant) replaces materializing all component lists for the
  report's summary line.
- **Stage-true forcing** — Step 5 (Analyze) fully evaluates the `Analysis`
  record (NFData deepseq) under its heap guard, so Steps 6–7 are rendering and
  IO only, and a heap exhaustion during analysis is reported as stage
  `analyze`, not `export`.
- **Report stage observability** — one INFO line with report render duration
  and the shared-analysis counts, so a regression of this class is visible in
  any run log.

## Capabilities

### New Capabilities
- `bounded-report-export`: Report/export run in bounded time and memory on
  inference-enriched graphs — single FGL conversion per run, shared
  connectivity artifacts, bounded top-N selection, analysis forced in its own
  stage. (Workflow 01 Steps 6–7; PRD §16.1 scale targets, §12 export formats)

### Modified Capabilities
- `report-consistency`: connectivity statistics (articulation points,
  biconnected-component count) and cohesion values in the report SHALL be the
  same values computed once during clustering/analysis — not independent
  recomputations that could diverge from the exported graph.
- `01-full-pipeline`: Stage 5 Analyze SHALL complete with a fully evaluated
  `Analysis`; Stages 6–7 SHALL perform no whole-graph analysis.

## Impact

- **Code**: `Domain.Analysis` (bounded top-N selection; `Analysis` gains
  articulation-points and bcc-count fields + NFData), `Domain.Graph.Analysis`
  (`biconnectedComponentCount`, cached variants reused),
  `UseCase.Pipeline.Core` (`clusterGraph` computes shared artifacts once,
  Step-5 deepseq of the full `Analysis`), `UseCase.Report` (pure rendering
  from provided values), `UseCase.Export` (subgraph pushes reuse shared
  articulation points), `UseCase.Analyze`.
- **API/outputs**: `GRAPH_REPORT.md` content unchanged (golden-tested against
  a HEAD-built baseline binary on a frozen corpus; top-N tie-breaking kept
  identical) with one deliberate exception: Bridge Nodes rows render in
  deterministic ascending NodeId order (previously fgl traversal order — same
  row set; see design D6), and on disconnected enriched graphs the
  articulation/bcc figures now cover every component (previously first
  component only). `graph.json` untouched; no new CLI flags.
- **Dependencies**: none new.
- **Performance target** (measured corpus above): Steps 6–7 complete in
  < 60 s with peak process RSS < 8 GB, versus 20+ min / 25.7 GB today.
  **Achieved**: whole run 69 s / 1.03 GB peak / exit 0; report render 0.008 s
  (see tasks 4.2).
- **Compatibility**: complements `fix-oom-memory-budget-guard` (which polices
  the footprint this change removes) and is orthogonal to
  `bounded-edge-inference` (which owns how many edges inference may add).
