## Context

Source: Mitchell & Mancoridis, *On the evaluation of the Bunch search-based
software modularization algorithm*, Soft Computing 12(1):77–93, 2008. Sections
2–3 define the MDG, MQ (TurboMQ) and the hill-climbing family; Section 4 is
the evaluation: 1,050 runs per system over η ∈ {0, 5, …, 100}% with and without
annealing (Table 1 systems: compiler 13/32, ispell 24/103, rcs 34/163, dot
42/256, swing 413/1,513 modules/relations), then 100 runs each on seven open
source systems and six random MDGs (Table 3), read as a structural landscape
(Figs. 9–10) and a similarity landscape (Table 4).

Graphos already has a deterministic Leiden (`Graphos.Domain.Community`, CSR
adjacency in unboxed vectors, `leidenCore`), `modularity`, cohesion, a
post-Leiden size cap, and strong determinism invariants
(INV-DETERMINISTIC-CLUSTER, permutation invariance T3, `DeterminismSpec`,
`PermutationSpec`). This change adds a second, randomized algorithm without
giving up those invariants, an algorithm-independent fitness, and the
multi-run evaluation.

## Pipeline

```
graph.json (any granularity)
   │  fold: node → nodeSourceFile; every relation except contains → file↔file
   │        weight = edge count (unit) | Σ edgeWeight (--bunch-weighted)
   ▼
file MDG  (undirected, weighted, CSR; n files, |E| pairs)
   │
   ├── Leiden partition folded to files (majority community)  ─┐  single point
   │                                                           │
   └── Bunch: seed s ──► random partition P₀ ──► hill climb ───┤  per run:
          (population p, η, SA T(0)/α, building blocks)        │  MQ, k, intra%
                                                               ▼
                               landscape: runs sorted by MQ, similarity
                               distribution of edges, random-partition
                               baseline, optional Rnd/Bip control MDG
   │
   ▼ (build only) best-of-population file partition → every symbol node
      → size-cap post-pass → joinCommunitiesToNodes → report + export
```

## Decisions

### D1 — Where the code lives (clean architecture)

| Piece | Layer | Module |
|---|---|---|
| Cluster factors, MQ, intra-edge fraction | Domain | `Graphos.Domain.Community.MQ` |
| SplitMix64 generator (pure, seeded) | Domain | `Graphos.Domain.Random` |
| File MDG fold, search state, neighbours, building blocks, hill climbing, annealing | Domain | `Graphos.Domain.Community.Bunch` |
| Per-run records, similarity distribution, random partitions, `Rnd`/`Bip` generators | Domain | `Graphos.Domain.Community.Landscape` |
| Algorithm dispatch, `ClusterQuality`, symbol push-back | UseCase | `Graphos.UseCase.Cluster` |
| Landscape orchestration (N runs, baseline, Leiden point) | UseCase | `Graphos.UseCase.ClusterEval` |
| Flags, config keys, `landscape.json`, report lines | Infrastructure | CLI parser, `app/Main.hs`, Export, Report |

Alternatives: putting MQ beside `modularity` in `Graphos.Domain.Community`
(rejected — that module is already 800 lines and MQ is reused by the
landscape without Leiden); a separate executable for the landscape (rejected —
reuses graph loading and the CLI's `--graph` conventions).

### D2 — The graph Bunch clusters

| Item | Decision |
|---|---|
| Level | File-level MDG: node → `nodeSourceFile`; the paper's modules are files (C) or classes (Java) |
| Edges | Every relation except `contains`, both directions folded to one undirected pair; self-pairs dropped |
| Weight | 1 per pair by default (the paper's default when the MDG carries no weights); `--bunch-weighted` sums `edgeWeight` over the folded edges |
| Cap | n ≤ `--bunch-max-files` (default 5,000); above it the stage logs the skip with n and the cap and runs Leiden |
| Push-back | Every symbol node receives its file's cluster; the size-cap post-pass then applies as for Leiden; `mergeSmallCommunities` does not (MQ already penalises singletons, and a forced merge would move the partition off the MQ optimum) |

Alternatives: clustering the full symbol graph (rejected — n·k neighbours per
iteration on 100k nodes is infeasible; the paper needed up to 35 M MQ
evaluations for 413 modules); directed ε_ij/ε_ji kept separately (rejected —
CF only ever uses their sum, so the undirected fold loses nothing); folding to
directories (rejected — that is the ground truth of
`clustering-evaluation-and-selection`, not the clustering input).

### D3 — MQ representation and incremental update

| Item | Decision |
|---|---|
| State | Unboxed vectors: `assign[i]` cluster index per file, `mu[c]` intra weight, `ext[c]` weight of edges with exactly one endpoint in c |
| `CF_c` | `0` if `mu[c] = 0`, else `2·mu[c] / (2·mu[c] + ext[c])`; `ext[c] = Σ_{j≠c}(ε_cj + ε_jc)` |
| `MQ` | `Σ_c CF_c`, held as a running total; a move of file i from a to b updates `mu`/`ext` of a and b from i's incident edges and recomputes two CFs — O(deg i) per candidate, O(1) in the number of clusters, as the paper's Sect. 2.2 optimisation |
| Full recompute | `modularizationQuality :: Graph -> CommunityMap -> MQResult` recomputes from scratch in O(|E|); used by the report and as the test oracle for the incremental path |
| Intra-edge fraction | `Σ_c mu[c] / Σ_e w_e` |
| Summation order | Clusters in ascending index; `Double` addition is not associative, so the order is fixed (same rule as `modularity`, INV-Q4) |

Alternatives: recomputing MQ per candidate (rejected — O(|E|) × n·k per
iteration); `Data.Map` cluster totals (rejected — the Leiden path already
showed the unboxed form is needed above 10k nodes; see the repo-root OOM
profile).

### D4 — The search

| Item | Decision |
|---|---|
| Start | k drawn uniformly from 1..n, each file assigned uniformly to one of k clusters — the paper observes uniformly distributed random cluster counts |
| Neighbour | P with exactly one file in a different cluster, including a new singleton; at most n·k per iteration |
| Building block | P plus one new cluster of exactly two files joined by an MDG edge; O(|E|) per iteration; the paper's remedy for MQ's reluctance to open clusters |
| Candidate order | Files by ascending index, target clusters by ascending index, then the new singleton, then building blocks by ascending (u, v) — canonical, so ties never depend on traversal |
| Threshold η | Examine at least ⌈η · |candidates|⌉ candidates; η = 0% takes the first strict improvement (NAHC); η = 100% takes the best of all (SAHC); in between, the best among those examined once the minimum is reached, continuing until one improves or all are seen. Default 0% — the paper found η affects runtime only |
| Annealing | Off by default. On: a non-improving candidate is accepted with `P(A) = exp(ΔMQ / T)` for `ΔMQ < 0`, `T(k+1) = α · T(k)` after every probability draw; defaults `T(0) = 100`, `α = 0.99` (the paper's best runtime reduction) |
| Convergence | No candidate improves MQ strictly (and, with annealing, none was accepted); plus an iteration cap `--bunch-max-iterations` (default 10,000) as a termination guarantee |
| Population | p independent seeds `s, s+1, …, s+p−1` (default p = 8); the build keeps the partition with the highest MQ, ties to the lowest seed |
| Output | Clusters relabelled 0..k−1 by ascending smallest member file, so equal partitions get equal labels |

Alternatives: the genetic algorithm (rejected — not evaluated in the paper);
accepting ties (rejected — breaks termination); restarts from the best
partition instead of fresh randoms (rejected — the paper's population is
independent restarts and the landscape needs independent samples).

### D5 — Determinism and randomness

| Item | Decision |
|---|---|
| Generator | SplitMix64 implemented in `Graphos.Domain.Random` (two `Word64` operations per draw, ~20 lines); `--seed N`, config `clustering.seed`, default 0 |
| Invariant | For fixed (graph, config, seed) the partition is identical across runs and machines; files are indexed in ascending `nodeSourceFile` order before any draw, so the same graph given in any node order yields the same partition (keeps T3 permutation invariance) |
| Landscape runs | Run r uses seed `s + r`; runs are independent as in the paper |

Alternatives: the `random`/`splitmix` packages (rejected for now — a new
dependency for 20 lines; revisit if a second consumer appears);
`System.Random` in Infrastructure feeding seeds down (rejected — the whole
search is Domain-pure and tests need to pin the stream).

### D6 — Landscape and similarity

| Item | Decision |
|---|---|
| Structural record per run | seed, start MQ, start cluster count, final MQ, final cluster count, intra-edge fraction, MQ evaluations, iterations, annealing acceptances |
| Ordering | Runs sorted by final MQ ascending, as Figs. 9–10 |
| Similarity | For every MDG edge (u, v): `C_uv` = number of runs with u, v in one cluster; `P_uv = C_uv / N`; buckets Zero `P = 0`, Low `0 < P ≤ 0.10`, Medium `0.10 < P < 0.75`, High `P ≥ 0.75`; each bucket reported as a percentage of |E| (Table 4) |
| Basins | Runs grouped by rounding MQ to 0.05; the report lists the groups holding ≥ 5% of runs with their MQ range and cluster-count range (the paper's "one or two basins") |
| Random-partition baseline | M random partitions (default 1,000) scored with MQ; min/median/max reported next to the runs (Fig. 7) |
| Leiden point | The build's Leiden partition folded to files by majority community, scored in the same coordinates |
| Control MDGs | `--random-graph rnd:N:D` and `bip:N:D`: N nodes, `round(D · N(N−1)/2)` distinct pairs chosen uniformly without replacement, bipartite across two halves of ⌊N/2⌋ and ⌈N/2⌉ for `bip`; seeded; the landscape then runs on that MDG instead of a graph file |

Alternatives: MoJo between runs (rejected — the paper reports it misbehaves
on isomorphic modules and uses the edge distribution instead); clustering the
Leiden partition N times (rejected — Leiden is deterministic, the landscape
would be one point repeated).

### D7 — CLI and configuration

| Surface | Decision |
|---|---|
| Build | `--cluster-algorithm leiden\|bunch` (default leiden), `--seed`, `--bunch-population`, `--bunch-threshold` (0–100), `--bunch-sa [T0,alpha]`, `--bunch-max-iterations`, `--bunch-max-files`, `--bunch-weighted` |
| Config | `clustering.algorithm`, `clustering.seed`, `clustering.bunch.{population, threshold, annealing: {enabled, t0, alpha}, maxIterations, maxFiles, weighted}`; CLI overrides config |
| Evaluate | `graphos clustereval landscape --graph G [--runs N] [--seed S] [--random-partitions M] [--random-graph rnd:N:D\|bip:N:D] [--json] [--out DIR]` writes `landscape.json` atomically and prints a markdown summary |
| Report | `GRAPH_REPORT.md` gains `Partition quality: MQ = …, modularity = …, intra-edge fraction = …` and the algorithm name |

Alternatives: a `--bunch` boolean (rejected — `clustering-evaluation-and-selection`
needs the same enum for `agglomerative|auto`); a separate `graphos bunch`
command (rejected — the cluster stage must stay the single place partitions
come from so the report and export agree).

## Risks / Trade-offs

- [Runtime on large file sets] → the file cap, η default 0%, population 8, and
  the iteration cap; the landscape prints MQ-evaluation counts so users see
  the cost before raising N.
- [Bunch partitions differ from Leiden's on the same repo] → expected; the
  report names the algorithm, Leiden stays the default, and the landscape
  places Leiden's point next to Bunch's basins so the difference is visible.
- [Randomness vs determinism invariants] → seeded SplitMix64 and canonical
  file indexing; `DeterminismSpec`-style tests pin the stream.
- [Unit weights hide strong couplings] → `--bunch-weighted`; the report states
  which was used.

## Verification

- `cabal build --flag dev` (-Werror) and `cabal test` green.
- Hspec + QuickCheck:
  - MQ: a cluster with 1 intra-edge and 3 inter-edges scores CF = 0.4 (the
    paper's Fig. 2, subsystem 2); `CF ∈ [0, 1]`, `MQ ∈ [0, k]`; a cluster
    with no external edges scores 1; the incremental update equals the full
    recompute after every move on random graphs (n ≤ 40).
  - Search: MQ never decreases without annealing; termination under the
    iteration cap; equal (graph, config, seed) → equal partition; node-order
    permutation → equal partition; the two-clique fixture converges to the
    two cliques from every seed in 0..31.
  - Landscape: bucket shares sum to 100% of |E|; `Rnd` and `Bip` generators
    produce exactly `round(D·N(N−1)/2)` pairs and `Bip` has no intra-half
    edge; similarity of N identical partitions is 100% High.
- Smoke, recorded in `tasks.md`: `graphos clustereval landscape` with 100
  runs on the Graphos repo at file granularity and on `rnd:100:0.05`,
  `rnd:100:0.5`, `bip:100:0.05`; the real graph is expected to show larger
  Zero + High than the dense random graph, the paper's headline finding.
