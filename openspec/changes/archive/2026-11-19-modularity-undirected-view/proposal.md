## Why

The `Partition quality` line of `GRAPH_REPORT.md` prints a modularity outside
the range modularity can take. On solario-core `origin/main` (`14d86be28`,
2026-11-12):

| Build | Reported `modularity` | Recomputed on the undirected support graph |
|---|---|---|
| `--granularity file --directed` (5,100 files) | **1.2149** | — |
| `src/infrastructure` sub-tree, `--directed`, Bunch | **1.2778** | — |
| `--granularity function --directed` (107,005 nodes, 292,680 edges) | 0.6485 | **0.3367** |

Modularity satisfies `-1/2 ≤ Q ≤ 1` (proved for Graphos's `Q` in
`formalization/lean/GraphosFormalization/Phase1/RangeBound.lean`,
`modularity_range_bound`). Two values break the bound outright. The third is in
range but about twice the true value: recomputing it in Python from
`graph.json` with the same formula reproduces 0.6485 exactly under
out-degrees, and gives 0.3367 under undirected degrees.

The cause is in `partitionQuality` (`src/Graphos/UseCase/Cluster.hs:273-283`),
which calls `modularity g cm` on the pipeline graph. That graph is directed
under `--directed`, and `computeCommunityStats`
(`src/Graphos/Domain/Community.hs:256-270`) then mixes two incompatible counts:

1. **Direction.** Degrees, `σ_tot` and `m` come from `neighbors`, which returns
   out-neighbours only when `gDirected g` (`Graph/Query.hs:62-66`). `σ_in`
   counts every edge record, whatever its direction. So `m` is roughly halved
   while `σ_in` is not. Leiden itself avoids this because
   `detectCommunitiesWithResolution` projects with `undirectedView` first
   (`Community.hs:96-108`). The report score never got the same projection.
2. **Orientation duplicates.** `gEdges` is keyed by the *ordered* pair
   `(source, target)`. One unordered pair stored as both `u → v` and `v → u`
   gives `u` and `v` one neighbour each, but adds `4` to `σ_in` instead of `2`.
   On solario-core, 10,265 of 282,415 unordered pairs (3.6 %) are stored in
   both orientations. Here the effect is small (0.3367 → 0.3360), but it alone
   pushes `Q` above `1` on small graphs (Lean witness `bothWays`, `Q = 3/2`).

The Phase 1 Lean model (`EdgeGraph`) assumes exactly what the implementation
does not guarantee: one record per unordered pair (`edges_noRev`) and
undirected degrees. The proven bound therefore does not cover the number the
report prints.

## What Changes

- **Report modularity on the simple undirected support graph.** `modularity`
  SHALL score a partition on the support pairs of the graph: each unordered
  `{u, v}` with `u ≠ v` and both endpoints in `gNodes`, counted once, ignoring
  edge direction and stored orientation. Degrees, `σ_tot`, `σ_in` and `m` are
  all derived from that one list.
- `partitionQuality` keeps calling `modularity`; the projection lives inside
  `modularity` so that no caller can score the directed graph again.
- **Lean arm.** New module `GraphosFormalization/Seam/UndirectedScoring.lean`
  (already written, builds with the pinned v4.14.0):
  - exact `Q = 3/2` witnesses of both defects;
  - `RecordGraph.toEdgeGraph`, which turns `gEdges` keys into a Phase 1
    `EdgeGraph` with every invariant proved;
  - `fixed_range_bound`: `-1/2 ≤ Q ≤ 1` for the fixed score, directed builds
    included;
  - `supportPairs_eq_self`: no change when no pair is stored twice.
- **Cross-check.** Two new rows in `crosscheck/q-lean.tsv` (`twoArcs`,
  `bothWays`) compared against Haskell `modularity`, as for the existing seven
  AVI-618 §5 fixtures.
- **No change to clustering.** In `src/`, `computeCommunityStats` is called
  only by `modularity`, and `modularity` only by `partitionQuality`, so Leiden
  and Bunch never read the changed code. Partitions stay byte-identical
  (`DeterminismSpec`, `PermutationSpec`). Only the printed `modularity` number
  changes.

## Non-goals

- Directed modularity (Leicht–Newman). Graphos clusters on the undirected
  projection, and the report scores what was clustered.
- Weighted modularity in the report line (unit weights, as today).
- The same build also printed per-community `Cohesion` values of 3.846 and
  3.571, against the `[0, 1]` range in the `community-detection` spec. That is
  a separate defect, to be filed as its own change; it is noted here only
  because the same run surfaced it.

## Capabilities

### Modified Capabilities

- `report-consistency`: the `Partition quality` modularity is scored on the
  simple undirected support graph and is always in `[-1/2, 1]`.
- `community-detection`: `modularity` is specified on the support pairs of the
  graph, independent of `gDirected` and of stored orientation.

## Impact

- Code: `src/Graphos/Domain/Community.hs` (`modularity`,
  `computeCommunityStats`, new `supportPairs`), `src/Graphos/UseCase/Cluster.hs`
  (comment only).
- Tests: `tests/Graphos/Domain/ModularityCrossCheckSpec.hs` (two rows),
  `CommunitySpec` (directed/orientation property), `ClusterSpec` (report value
  in range on a `--directed` fixture).
- Lean: `formalization/lean/GraphosFormalization/Seam/UndirectedScoring.lean`,
  root import, README row, `Harness/CrossCheck.lean` rows.
- Output: the printed `modularity` of every `--directed` build changes, and so
  does that of any undirected build whose `gEdges` holds both orientations of a
  pair. Recorded `GRAPH_REPORT.md` baselines that quote it must be refreshed.
