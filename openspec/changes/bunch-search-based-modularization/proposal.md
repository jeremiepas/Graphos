## Why

Graphos clusters every graph with Leiden and reports no quality number for the
result beyond Leiden's own objective (modularity), so a partition cannot be
judged, compared across runs, or compared with another algorithm. Mitchell &
Mancoridis, *On the evaluation of the Bunch search-based software
modularization algorithm* (Soft Computing 12(1):77–93, 2008,
doi:10.1007/s00500-007-0218-3) contributes two things Graphos lacks:

1. **A fitness function for module partitions.** Bunch scores a partition of a
   module dependency graph (MDG) with *modularization quality*, the sum of one
   cluster factor per cluster: `MQ = Σ CF_i`, where `CF_i = 0` when cluster i
   has no intra-edges and otherwise
   `CF_i = 2µ_i / (2µ_i + Σ_{j≠i}(ε_ij + ε_ji))` (µ_i = intra-edge weight of
   cluster i, ε_ij = weight of edges from cluster i to cluster j, unit weights
   when the MDG carries none). `CF_i ∈ [0, 1]`, `MQ ∈ [0, k]`. Moving one node
   changes exactly two cluster factors, so MQ updates in O(1) per move. MQ is
   algorithm-independent: it scores Leiden's partition as readily as Bunch's.
2. **A search algorithm and a way to evaluate it.** Bunch hill-climbs from
   random partitions over *neighbouring partitions* (one node moved to another
   cluster or to a new singleton) and *building-block partitions* (a new
   two-node cluster joined by an MDG edge), with a threshold η choosing between
   next-ascent (η = 0%) and steepest-ascent (η = 100%), an optional
   simulated-annealing acceptance `P(A) = e^{ΔMQ/T}` for `ΔMQ < 0` with
   `T(k+1) = α·T(k)`, and a population of random starting points. The paper
   evaluates the search by running it many times and reading the **search
   landscape**: per-run MQ and cluster count, the intra-edge fraction
   `|IntraEdges| / |E|`, the random start against the converged result, and an
   **edge similarity distribution** — the share of runs in which each edge is
   intra-cluster, bucketed Zero (0%), Low (0–10%], Medium (10–75%), High
   [75%–100%]. Its findings are the acceptance tests of this change: η changes
   runtime, not quality; simulated annealing with `T(0) = 100`, α ∈ {0.99,
   0.9, 0.8} cut MQ evaluations 10–32% without raising MQ; real systems
   concentrate in one or two basins of attraction with large Zero + High
   similarity, while random graphs of the same size and density put ≥ 25% of
   their edges in Medium.

No PRD.md exists in this repository; section references below point to the
`community-detection` spec, which cites PRD §5.1–§5.4.

## What Changes

- **Modularization quality**: a pure `modularizationQuality` over any
  `CommunityMap`, with the per-cluster factors and the intra-edge fraction,
  computed on the undirected projection with unit edge weights by default and
  `edgeWeight` on request. The cluster stage records MQ, modularity and
  intra-edge fraction for the final partition; `GRAPH_REPORT.md` prints them.
- **Bunch clustering**: `--cluster-algorithm bunch` on the build. It runs on
  the file-level MDG (symbol nodes folded to their source file; the file
  partition is pushed back to every symbol), is seeded and deterministic for a
  given seed, and exposes the population size, η, simulated annealing
  (`T(0)`, α) and the file cap. Above the cap (default 5,000 files) the stage
  reports the skip and runs Leiden instead.
- **Search landscape evaluation**: `graphos clustereval landscape` runs the
  seeded search N times (default 100, the paper's count) on a graph, writes
  the structural landscape (per-run MQ, cluster count, intra-edge fraction,
  random-start MQ and cluster count), the similarity distribution of edges,
  and a random-partition baseline (default 1,000 partitions, the paper's
  count); it can generate the paper's random (`Rnd`) and bipartite (`Bip`)
  MDGs at a chosen density as a control. Leiden's partition is placed as a
  single point in the same coordinates.

## Non-goals

- Bunch's subsystem hierarchy (clusters of clusters). One level is produced;
  the paper itself evaluates the lowest level.
- Omnipresent-module isolation (Müller). Generated/vendored exclusion and
  `--max-community-fraction` remain the tools for degenerate hubs.
- The genetic-algorithm search (Doval et al. 1999). The paper evaluates hill
  climbing only.
- MoJo / EdgeSim / MeCl. The paper cites them as related work and uses none.
  `clustering-evaluation-and-selection` carries MoJoFM.
- Making Bunch the default. Leiden stays the default and its partitions are
  byte-identical unless `--cluster-algorithm bunch` is passed.

## Capabilities

### New Capabilities
- `modularization-quality`: cluster factors, MQ, intra-edge fraction; recorded
  for every clustered build (workflow 01 stage 4, PRD §5.4).
- `bunch-clustering`: seeded hill-climbing search over the file-level MDG
  (workflow 01 stage 4, PRD §5.1).
- `search-landscape-evaluation`: `graphos clustereval landscape`, random
  baselines, similarity distribution (workflow 04 query tooling, PRD §5.1).

### Modified Capabilities
- `community-detection`: the algorithm becomes selectable
  (`--cluster-algorithm leiden|bunch`, config `clustering.algorithm`), Leiden
  remaining the default (PRD §5.1).
- `report-consistency`: the report prints the partition's MQ, modularity and
  intra-edge fraction from the same `CommunityMap` the export writes.

Coordination: the uncommitted `clustering-evaluation-and-selection` change
introduces the same `--cluster-algorithm` flag with `agglomerative|auto` and a
`clustereval` command with `score|compare|sweep`. Whichever lands second
extends the other's enum and subcommand list; Bunch then becomes a sweep
candidate and MQ a sweep column. That change lists Bunch as a non-goal on the
strength of E-SC4R's MoJoFM scores against directory truth. This change makes
no claim that Bunch beats Leiden: it adds the fitness function and the
stability evaluation the paper is about, which apply to Leiden too.

## Impact

- **Domain** (pure): `Graphos.Domain.Community.MQ` (cluster factors, MQ,
  intra-edge fraction), `Graphos.Domain.Random` (SplitMix64, no new package),
  `Graphos.Domain.Community.Bunch` (file MDG fold, search state, neighbours,
  building blocks, hill climbing, annealing), `Graphos.Domain.Community.Landscape`
  (per-run records, similarity distribution, random partitions, `Rnd`/`Bip`
  generators).
- **UseCase**: `UseCase/Cluster.hs` dispatches on the algorithm and returns a
  `ClusterQuality` next to the partition; `UseCase/ClusterEval.hs` runs the
  landscape.
- **Infrastructure / CLI**: build flags `--cluster-algorithm`, `--seed`,
  `--bunch-population`, `--bunch-threshold`, `--bunch-sa`, `--bunch-max-files`,
  `--bunch-weighted`; `clustereval landscape` with `--runs`, `--seed`,
  `--random-partitions`, `--random-graph`, `--json`; `landscape.json`
  (atomic write) and a markdown section; two report lines.
- **Cost**: one hill-climbing iteration examines up to `n·k` neighbours and
  O(|E|) building blocks at O(1) each. The paper's 413-module Swing took up to
  35 M MQ evaluations per run, which is why the file cap and the default
  population bound the stage. `leiden` builds do no extra work beyond one
  O(|E|) MQ score.
