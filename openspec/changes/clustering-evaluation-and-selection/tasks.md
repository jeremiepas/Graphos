## 1. Partition foundation (Domain)

- [x] 1.1 `Graphos.Domain.Community.Partition`: `Partition` type, restriction to common nodes, meet of partitions. Check: QuickCheck — meet is commutative, idempotent, and refines both inputs
  - `Graphos.Domain.Community.Partition`: `Partition` (element → group label), `fromAssignment`/`fromGroups`/`groups`, `restrictCommon` (with dropped counts), `meet`/`meetAll`, `refines`, `samePartition`. `PartitionSpec`: meet commutative, idempotent, refines both (QuickCheck).
- [x] 1.2 Directory ground truth (depth d, multi-ref meet, JSON truth loader in Infrastructure). Check: the three `clustering-evaluation` ground-truth scenarios pass
  - `directoryGroup`/`directoryTruth` (depth `d`, leading `./` ignored, shallow files keep their directory); multi-ref truth = `meetAll` with dropped count; `Graphos.Infrastructure.ClusterEval.TruthFile` reads `--truth FILE` (`{"file": "group"}`, non-string group rejected). The three spec scenarios pass.
- [x] 1.3 MoJo distance (tagging + maximum bipartite matching) and MoJoFM. Check: identity = 0 / 100, range [0, 100], brute-force maximum for n ≤ 6 equals the closed form
  - `mno` (optimal one-way MoJo: maximal-overlap tagging, Kuhn matching, `M + l − g`), `maxMno` (closed form `n − h`, see design D3), `mojoFM` with common/dropped counts. Identity 0 / 100, range [0, 100] (QuickCheck), brute force for every reference of n ≤ 6 equals the closed form, worked examples.
- [x] 1.4 Stability (both directions). Check: equal partitions → 100 / 100
  - `stability old new = (MoJoFM(old,new), MoJoFM(new,old))`; equal partitions 100 / 100 (QuickCheck).

## 2. Agglomerative clustering (Domain) — parallel with 1.2–1.4

- [x] 2.1 Build W from the Bunch MDG (`foldToFileMDGWith (clustering.bunch.relations) EdgeWeights`, unboxed upper triangle) — no second fold. Check: the three file-dependency-matrix scenarios
  - `Agglomerative.fileMatrix rels = foldToFileMDGWith rels EdgeWeights`; feature rows (`featureRow`) add the self entry. The three scenarios pass on built graphs.
- [x] 2.2 Distances (euclidean, manhattan, cosine, zero-row rule). Check: hand-computed 3×3 fixture
  - `rowDistance` over sparse rows (merge of ascending indices): euclidean, manhattan, cosine (`1 − cos`, 1 for a zero row). Hand-computed 4-file fixture; symmetric, non-negative (QuickCheck).
- [x] 2.3 Single linkage (planned: Prim MST + cut at K). Check: two-group fixture, exact K, determinism
  - Single linkage runs the same generic algorithm as 2.4 (Lance–Williams `min`): a Prim MST cut broke the cluster-id tie rule at the cut (QuickCheck counterexample), see design D4. Two-group fixture for all distances, F = 20 / q = 5 → 4 clusters, equals a naive single-linkage merge (2,000 cases).
- [x] 2.4 Average and complete linkage (planned: nearest-neighbour chain). Check: matches a naive O(F³) reference on random graphs F ≤ 30 (QuickCheck)
  - Generic algorithm with cached nearest neighbours (not the NN chain, which reorders tied merges). Equals the naive O(F³) reference for average and complete × 3 distances on random graphs F ≤ 30 (2,000 cases); Lance–Williams = mean / max of pairwise distances; deterministic; exactly `min K F` clusters.
- [ ] 2.5 File-count cap: skip to Leiden with a reported skip, mirroring `BunchSkip`. Check: cap scenario. (Lives in the `runClustering` dispatch, done with 3.2.)

## 3. Orchestration (UseCase)

- [ ] 3.1 `Graphos.UseCase.ClusterEval`: score / compare / sweep, ranking with tie-breaks. Check: ranking scenarios
- [ ] 3.2 `UseCase/Cluster.hs`: extend the existing `ClusterAlgorithm` (`Leiden | BunchAlgorithm`) with agglomerative and auto in `runClustering`; `auto` applies the winner (Leiden, Bunch or agglomerative) to node communities. Check: default-unchanged scenario (Leiden partition byte-identical), winner-applied scenario

## 4. CLI and outputs (Infrastructure)

- [ ] 4.1 `clustereval` subcommands `score|compare|sweep` next to the existing `landscape` (`--graph`, `--ref-graph`, `--truth-depth`, `--truth`, `--json`). Check: JSON keys of the score and compare scenarios
- [ ] 4.2 Extend the existing `--cluster-algorithm` / `clustering.algorithm` vocabulary (`agglomerative`, `auto`, via the token table of `cli-enum-flag-vocabulary` when it lands) and add `--linkage`, `--distance`, `--cluster-divisor`, `--agglomerative-max-files`; config key `cluster.algorithm`. Check: CLI parser tests
- [ ] 4.3 `cluster_eval.json` (atomic write) and a markdown ranking section in `GRAPH_REPORT.md`. Check: file present after an `auto` build

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` (-Werror) and `cabal test` green
- [ ] 5.2 Smoke: `graphos clustereval sweep` on the Graphos repo (file granularity) and on a solario-core graph; record the top 5 rows and Leiden's rank here
- [ ] 5.3 `openspec validate clustering-evaluation-and-selection --strict`
