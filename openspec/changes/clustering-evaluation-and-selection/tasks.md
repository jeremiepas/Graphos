## 1. Partition foundation (Domain)

- [ ] 1.1 `Graphos.Domain.Community.Partition`: `Partition` type, restriction to common nodes, meet of partitions. Check: QuickCheck — meet is commutative, idempotent, and refines both inputs
- [ ] 1.2 Directory ground truth (depth d, multi-ref meet, JSON truth loader in Infrastructure). Check: the three `clustering-evaluation` ground-truth scenarios pass
- [ ] 1.3 MoJo distance (tagging + maximum bipartite matching) and MoJoFM. Check: identity = 0 / 100, range [0, 100], brute-force maximum for n ≤ 6 equals the closed form
- [ ] 1.4 Stability (both directions). Check: equal partitions → 100 / 100

## 2. Agglomerative clustering (Domain) — parallel with 1.2–1.4

- [ ] 2.1 Fold graph to files and build W (unboxed upper triangle). Check: the two file-dependency-matrix scenarios
- [ ] 2.2 Distances (euclidean, manhattan, cosine, zero-row rule). Check: hand-computed 3×3 fixture
- [ ] 2.3 Single linkage via Prim MST + cut at K. Check: two-group fixture, exact K, determinism
- [ ] 2.4 Average and complete linkage via nearest-neighbour chain. Check: matches a naive O(F³) reference on random graphs F ≤ 30 (QuickCheck)
- [ ] 2.5 File-count cap with reported skip. Check: cap scenario

## 3. Orchestration (UseCase)

- [ ] 3.1 `Graphos.UseCase.ClusterEval`: score / compare / sweep, ranking with tie-breaks. Check: ranking scenarios
- [ ] 3.2 `UseCase/Cluster.hs` dispatch on `ClusterAlgorithm`; `auto` applies the winner to node communities. Check: default-unchanged scenario (Leiden partition byte-identical), winner-applied scenario

## 4. CLI and outputs (Infrastructure)

- [ ] 4.1 `clustereval` subcommand (`score|compare|sweep`, `--graph`, `--ref-graph`, `--truth-depth`, `--truth`, `--json`). Check: JSON keys of the score and compare scenarios
- [ ] 4.2 Build flags `--cluster-algorithm`, `--linkage`, `--distance`, `--cluster-divisor`, `--agglomerative-max-files`; config key `cluster.algorithm`. Check: CLI parser tests
- [ ] 4.3 `cluster_eval.json` (atomic write) and a markdown ranking section in `GRAPH_REPORT.md`. Check: file present after an `auto` build

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` (-Werror) and `cabal test` green
- [ ] 5.2 Smoke: `graphos clustereval sweep` on the Graphos repo (file granularity) and on a solario-core graph; record the top 5 rows and Leiden's rank here
- [ ] 5.3 `openspec validate clustering-evaluation-and-selection --strict`
