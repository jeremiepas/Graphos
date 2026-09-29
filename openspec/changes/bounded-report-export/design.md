## Context

Workflow 01 Steps 6–7 render `GRAPH_REPORT.md` and drive exports after
clustering. Profiling on 2026-09-29 (repo root, 16.8k nodes / 410k edges after
inference / 893 communities, run under the new memory-budget-guard's derived
30.6 GiB cap) showed Steps 1–5 at ~21 s and Steps 6–7 at 20+ minutes and up
to 25.7 GB RSS (~24 CPU-min, GC-bound) before the run was killed — the same
allocation profile as the 2026-09-22 kernel OOM. `graph.json` is *not* the
problem: it is already streamed incrementally (see the exportAll note about
avoiding a JSON AST). The problem is whole-graph analysis recomputed inside
the report/export stage:

| Recomputation | Where | Already available as |
|---|---|---|
| `articulationPoints g` (fresh `toCachedFGL`) | `generateReport`, Neo4j subgraph push, Memgraph subgraph push | computed in `clusterGraph` for aggregates |
| `biconnectedComponents g` (fresh `toCachedFGL`, all lists) | `generateReport` (prints only the count) | — (count never precomputed) |
| `cohesionScore g members` × 893 | `communitiesSection`, `lowCohesionQuestions` | `analysisCohesion` (`scoreAllCohesion` from clustering) |
| `surprisingConnections` / `suggestQuestions` O(E) list builds + sorts | forced lazily at render time (Step 7) | should be forced in Step 5 (Analyze) |

Layer constraints: selection/counting logic is pure Domain
(`Domain.Analysis`, `Domain.Graph.Analysis`); orchestration of what is
computed when is UseCase (`Pipeline.Core`, `Report`, `Export`). No IO changes.

## Goals / Non-Goals

**Goals:**
- Exactly one FGL conversion of the enriched graph per run; connectivity
  artifacts computed once and shared by report, aggregates, and subgraph pushes
- Report rendering is O(output size) plus one pass over precomputed values —
  no whole-graph traversal in Steps 6–7
- The Analyze stage owns (and heap-guard-attributes) all analysis cost
- `GRAPH_REPORT.md` bytes unchanged on existing fixtures

**Non-Goals:**
- Reducing the inferred-edge count itself (`bounded-edge-inference` owns
  density semantics; 392k edges at Normal density is a separate discussion)
- HTML viewer export internals (`--no-viz` was set in the incident; the HTML
  path receives the shared values but its rendering is untouched)
- Memory budget policing (delivered by `fix-oom-memory-budget-guard`)
- Streaming the report to disk (it is ~100 KB; the graph analysis, not the
  markdown, is the cost)

## Decisions

### D1 — Share one `CachedFGL` per run; compute connectivity once

`clusterGraph` already builds the enriched graph and computes articulation
points for aggregates. It becomes the single owner: build `toCachedFGL` once,
derive `articulationPointsWithCached` and `biconnectedComponentCountWithCached`
from it, and drop the ad-hoc `articulationPoints g` call sites in
`generateReport` and the subgraph pushes.

**Alternatives considered:**
- **A: memoize inside `Graph` (lazy field)** — hidden shared state, breaks
  value semantics of Domain types and NFData forcing; rejected.
- **B: global cache keyed by graph hash** — cross-run complexity for a
  per-run problem; rejected.

### D2 — Carry shared artifacts on the `Analysis` record

`Analysis` gains `analysisArticulation :: [NodeId]` and
`analysisBccCount :: Int`. `Analysis` already flows to `generateReport`,
`exportAll` (which hosts the subgraph pushes), and the HTML exporter, so no
signature threading beyond `analyzeGraph`'s construction site.

**Alternatives considered:**
- **A: extend `ClusterOutput` only** — `exportAll` receives `Analysis`, not
  `ClusterOutput`; would force new parameters through the ExportPort; rejected.
- **B: new record passed alongside** — same threading cost with a second
  type; rejected.

### D3 — Bounded top-N selection in Domain.Analysis

`surprisingConnections` / `suggestQuestions` currently build the full O(E)
candidate list and sort it to take 3–7 items. Replace with a strict bounded
accumulator (`selectTopNOn :: Ord k => Int -> (a -> k) -> [a] -> [a]`,
size-capped, same comparator, first-occurrence-wins on ties) so peak
allocation is O(N), not O(E). Tie behavior is pinned by a property test
against the `sortOn`+`take` reference so report bytes do not change.

**Alternatives considered:**
- **A: keep full sort, rely on laziness** — `sortOn` forces the whole list;
  rejected by the measurement.
- **B: sample edges** — changes report content; rejected.

### D4 — Force the full `Analysis` in Step 5 (Analyze)

Add NFData to `Analysis` (and its leaf types where missing) and `deepseq` the
record inside the existing `withHeapGuard … "analyze"` block. Steps 6–7 then
render pre-evaluated values, and heap exhaustion during analysis is reported
as stage `analyze` (truthful attribution for the memory-budget-guard's
stage-named failure).

**Alternatives considered:**
- **A: leave lazy** — today's behavior: 20 minutes of "export" that is
  actually analysis; rejected.
- **B: force in Step 7 before rendering** — right cost, wrong stage name;
  rejected.

### D5 — Count-only biconnected components

`biconnectedComponentCount(WithCached) :: … -> Int` walks the same DFS but
only counts components; the report never needs the member lists. The
list-returning function remains for existing callers.

### D6 — Own Tarjan connectivity pass replaces fgl `ap`/`bcc` on the hot path
*(added during implementation, from measurement)*

Micro-benchmarking the real 409k-edge enriched graph showed fgl's `ap` at
**164 s** (inductive `match` decomposition copies the Patricia tree per DFS
step) and fgl's `bcc` allocating an embedded subgraph per block — together
the actual cause of the 25.7 GB / 20-min stage, dwarfing everything else
(toCachedFGL 0.7 s, bounded surprises 0.9 s, godNodes/questions < 0.1 s).
`connectivityWithCached :: CachedFGL -> ([NodeId], Int)` computes articulation
points and the block count in one iterative ST-vector Tarjan pass (< 1 s on
the same graph), covering **all** connected components — fgl's `ap`/`bcc` are
documented connected-graph-only and silently skip or lump further components.
`articulationPointsWithCached` and `biconnectedComponentCountWithCached`
delegate to it; the fgl-based `biconnectedComponents` (member lists) remains
for list consumers.

Two deliberate output consequences, both verified on the frozen-corpus golden:
- Bridge-node rows now render in deterministic ascending NodeId order (fgl's
  order was a Patricia-traversal artifact); same row set on connected graphs
  — a pure permutation of the section.
- On graphs whose enriched form is disconnected, articulation points and the
  block count now cover every component (previously: first component only /
  lumped) — the more correct figure, aligned with report-consistency's
  multi-component scenario.

## Risks / Trade-offs

- [Report byte-diff from selection reordering] → same comparator + pinned tie
  behavior + golden test on a fixture graph; any diff fails CI.
- [NFData on `Analysis` forces fields some path never used] → the report
  renders every field anyway; HTML uses a subset but the values are now cheap
  (precomputed); acceptable.
- [Carrying `analysisArticulation` grows the record] → `[NodeId]` of
  articulation points is small (hundreds) relative to the graph; negligible.
- [Subgraph pushes previously recomputed on a *possibly different* graph
  value than the report] → sharing removes a latent consistency bug
  (report-consistency capability), but any code that relied on push-time
  recomputation of a mutated graph must now pass the right `Analysis`;
  reviewed at the two push call sites.

## Migration Plan

1. Domain: `selectTopNOn` + property tests; `biconnectedComponentCount`
   (+WithCached) + equality tests (no behavior change yet).
2. `Analysis` record extension + NFData; `analyzeGraph`/`clusterGraph`
   compute-once wiring; Step-5 deepseq under the analyze heap guard.
3. `generateReport` / `exportAll` consume shared values; delete recomputation
   call sites; golden-test the report.
4. Verification run on the repo-root corpus with `--rts-profile`; record
   Steps 6–7 wall time and peak RSS in tasks notes.
5. Rollback: each step is independently revertible; the shared fields are
   additive.

Verification: `cabal build` clean under `-Werror`; `cabal test` green
including new selection/count/golden specs; profiled repo-root run meets the
< 60 s / < 8 GB target for Steps 6–7.

## Open Questions

- None blocking. Whether Normal edge density should be auto-downgraded on
  100k+-edge graphs is deliberately left to `bounded-edge-inference`.
