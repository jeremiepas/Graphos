## 1. Partition foundation (Domain)

- [x] 1.1 `Graphos.Domain.Community.Partition`: `Partition` type, restriction to common nodes, meet of partitions. Check: QuickCheck — meet is commutative, idempotent, and refines both inputs
  - `Partition` (element → group label), `fromAssignment`/`fromGroups`/`groups`, `restrictCommon` (with dropped counts), `meet`/`meetAll`, `refines`, `samePartition`. `PartitionSpec`: meet commutative, idempotent, refines both (QuickCheck).
- [x] 1.2 Directory ground truth (depth d, multi-ref meet, JSON truth loader in Infrastructure). Check: the three `clustering-evaluation` ground-truth scenarios pass
  - `directoryGroup`/`directoryTruth` (depth `d`, leading `./` ignored, shallow files keep their directory); multi-ref truth = `meetAll` with dropped count; `Graphos.Infrastructure.ClusterEval.TruthFile` reads `--truth FILE` (`{"file": "group"}`, non-string group rejected). Spec scenarios pass.
- [x] 1.3 MoJo distance (tagging + maximum bipartite matching) and MoJoFM. Check: identity = 0 / 100, range [0, 100], brute-force maximum for n ≤ 6 equals the closed form
  - `mno` (optimal one-way MoJo: maximal-overlap tagging, Kuhn matching, `M + l − g`), `maxMno` (closed form `n − h`, design D3), `mojoFM` with common/dropped counts. Identity 0 / 100, range [0, 100] (QuickCheck), brute force for every reference of n ≤ 6 equals the closed form, worked examples.
- [x] 1.4 Stability (both directions). Check: equal partitions → 100 / 100
  - `stability old new = (MoJoFM(old,new), MoJoFM(new,old))`; equal partitions 100 / 100 (QuickCheck).

## 2. Agglomerative clustering (Domain) — parallel with 1.2–1.4

- [x] 2.1 Fold graph to files and build W (unboxed upper triangle). Check: the two file-dependency-matrix scenarios
  - `Agglomerative.fileMatrix rels = foldToFileMDGWith rels EdgeWeights` — the Bunch MDG with edge weights, no second fold. Feature rows (`featureRow`) add the self entry. The three file-dependency-matrix scenarios pass.
- [x] 2.2 Distances (euclidean, manhattan, cosine, zero-row rule). Check: hand-computed 3×3 fixture
  - `rowDistance` over sparse rows (merge of ascending indices): euclidean, manhattan, cosine (`1 − cos`, 1 for a zero row). Hand-computed 4-file fixture; symmetric, non-negative (QuickCheck).
- [x] 2.3 Single linkage via Prim MST + cut at K. Check: two-group fixture, exact K, determinism
  - Single linkage runs the generic Lance–Williams (`min`) algorithm: a Prim MST cut broke the cluster-id tie rule at the cut (QuickCheck counterexample), see design D4. Two-group fixture for all distances, F = 20 / q = 5 → 4 clusters, equals a naive single-linkage merge (2,000 cases).
- [x] 2.4 Average and complete linkage via nearest-neighbour chain. Check: matches a naive O(F³) reference on random graphs F ≤ 30 (QuickCheck)
  - Generic algorithm with cached nearest neighbours (not the NN chain, which reorders tied merges). Equals the naive O(F³) reference for average and complete × 3 distances on random graphs F ≤ 30 (2,000 cases); Lance–Williams = mean / max of pairwise distances; deterministic; exactly `min K F` clusters.
- [x] 2.5 File-count cap with reported skip. Check: cap scenario
  - `AgglomerativeAlgorithm` checks `--agglomerative-max-files` (default 10,000) before building the matrix; above it `runClustering` falls back to Leiden with `cqSkip = Just (BunchSkip files cap)`. `ClusterSpec` covers the cap scenario (6 files, cap 2 → Leiden + skip) and the exact-K agglomerative run.

## 3. Orchestration (UseCase)

- [x] 3.1 `Graphos.UseCase.ClusterEval`: score / compare / sweep, ranking with tie-breaks. Check: ranking scenarios
  - `scoreGraph` (stored partition folded to files vs truth), `compareGraphs` (stability both ways), `sweepGraph` (Leiden + Bunch + 3 distances × 3 linkages × divisors 5/7/10/15/20/25). Rank by MoJoFM desc, then fewer singletons, then config name. `ClusterEvalSpec` checks the order, the 11-row reduced grid, and that the winner's partition covers every node.
- [x] 3.2 `UseCase/Cluster.hs` dispatch on `ClusterAlgorithm`; `auto` applies the winner to node communities. Check: default-unchanged scenario (Leiden partition byte-identical), winner-applied scenario
  - `ClusterAlgorithm` = `Leiden | BunchAlgorithm | AgglomerativeAlgorithm | AutoAlgorithm`. Leiden path is unchanged (`ClusterSpec` asserts byte-identical `clusterGraphWithResolution`). `auto` runs `sweepGraph` in the pipeline, re-runs the winning algorithm on the graph, and carries the full report in `analysisClusterEval`; the applied partition is the winner (`agglo`/`auto` tests).

## 4. CLI and outputs (Infrastructure)

- [x] 4.1 `clustereval` subcommand (`score|compare|sweep`, `--graph`, `--ref-graph`, `--truth-depth`, `--truth`, `--json`). Check: JSON keys of the score and compare scenarios
  - `graphos clustereval score|compare|sweep` parse `--graph` (repeatable for compare), repeated `--ref-graph`, `--truth-depth`, `--truth`, `--json`. `scoreJSON` emits `mojofm`, `truth_groups`, `clusters`, `dropped`; `compareJSON` emits `stability_ab` / `stability_ba`. `ClusterEvalSpec` and `ParserSpec` cover both.
- [x] 4.2 Build flags `--cluster-algorithm`, `--linkage`, `--distance`, `--cluster-divisor`, `--agglomerative-max-files`; config key `cluster.algorithm`. Check: CLI parser tests
  - `clustering.algorithm` accepts `leiden|bunch|agglomerative|auto`; `clustering.agglomerative.{linkage,distance,divisor,max_files}` decode with defaults (single, cosine, 25, 10000); the four flags are in `clusteringOverridesOpts`. `ParserSpec` parses them and rejects `ward` / divisor 0.
- [x] 4.3 `cluster_eval.json` (atomic write) and a markdown ranking section in `GRAPH_REPORT.md`. Check: file present after an `auto` build
  - `Graphos.UseCase.Export` writes `cluster_eval.json` through the new atomic `epExportClusterEval` port whenever `analysisClusterEval` is present; `generateReport` renders the same ranking under `## Configuration sweep`. Verified on an `--cluster-algorithm auto` build: both the file and the section are present.

## 5. Verification

- [x] 5.1 `cabal build --flag dev` (-Werror) and `cabal test` green
  - `cabal build --flag dev` passes; `cabal test --flag dev` passes (1785 examples, 0 failures, 4 pending) with `LANG=C.UTF-8` (the locale otherwise breaks an unrelated Unicode arrow in the focus suite).
- [x] 5.2 Smoke: `graphos clustereval sweep` on the Graphos repo (file granularity) and on a solario-core graph; record the top 5 rows and Leiden's rank here
  - Graphos repo build (`--granularity file`, 393 MDG files, 26 truth groups). Top 5 and Leiden's rank (11) below. A solario-core graph was not present in the sandbox filesystem, so that half of the smoke could not be run; this is recorded rather than guessed.

    | Rank | Configuration | MoJoFM | Clusters | Singletons | Modularity |
    |------|---------------|--------|----------|------------|------------|
    | 1 | cosine_complete_7 | 54.43 | 1407 | 52 | 0.0037 |
    | 2 | cosine_complete_5 | 48.70 | 1429 | 74 | 0.0037 |
    | 3 | cosine_average_7 | 46.61 | 1407 | 56 | 0.0081 |
    | 4 | cosine_single_7 | 46.61 | 1407 | 56 | 0.0081 |
    | 5 | cosine_average_5 | 46.61 | 1429 | 78 | 0.0083 |
    | 10 | leiden | 45.31 | 243 | 38 | 0.5684 |

    Selected: `cosine_complete_7`. Leiden ranks 10 of 56 configurations.
- [x] 5.3 `openspec validate clustering-evaluation-and-selection --strict`
  - `openspec validate clustering-evaluation-and-selection --strict` passes (`Change ... is valid`) after trimming the four long requirement texts.
