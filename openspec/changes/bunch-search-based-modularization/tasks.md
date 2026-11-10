## 1. Modularization quality (Domain)

- [ ] 1.1 `Graphos.Domain.Community.MQ`: `MQResult` (MQ, per-cluster CF, intra-edge fraction, cluster count), `modularizationQuality :: Graph -> CommunityMap -> MQResult` on the undirected projection, unit weights, fixed ascending summation order. Check: a cluster with 1 intra-edge and 3 inter-edges gives CF = 0.4 (paper Fig. 2, subsystem 2); QuickCheck `CF ∈ [0,1]`, `MQ ∈ [0,k]`; a closed cluster scores 1; empty graph scores 0
- [ ] 1.2 Weighted variant (`edgeWeight` sums). Check: doubling every weight leaves MQ unchanged; one heavy intra-edge raises its cluster's CF

## 2. Random source and file MDG (Domain) — parallel with 1

- [ ] 2.1 `Graphos.Domain.Random`: SplitMix64 (`next :: Gen -> (Word64, Gen)`, `uniformInt`, `uniformDouble`, `split`). Check: the reference SplitMix64 test vector for seed 0 (first three outputs) matches; `uniformInt n` is in `[0, n)` for all n > 0
- [ ] 2.2 `Graphos.Domain.Community.Bunch.foldToFileMDG`: node → `nodeSourceFile`, all relations except `contains`, undirected, self-pairs dropped, unit or summed weights, files indexed in ascending path order, CSR adjacency. Check: two files with 3 symbol edges in both directions fold to one pair of weight 1 (unit) / sum (weighted); `contains`-only graph folds to zero edges; node-order permutation yields identical CSR

## 3. Bunch search (Domain) — depends on 1, 2

- [ ] 3.1 Search state with `assign`, `mu`, `ext`, running MQ; `moveFile` updating two clusters; `fromRandomPartition gen`. Check: after every random move on random MDGs (n ≤ 40) the running MQ equals `modularizationQuality` of the materialised partition (QuickCheck)
- [ ] 3.2 Candidate enumeration: neighbours (other clusters, new singleton) then building blocks, canonical order. Check: a 3-file, 2-cluster state enumerates exactly the expected candidate list in order; building blocks only pair edge-connected files
- [ ] 3.3 Hill climbing with η (NAHC at 0, SAHC at 100, minimum-examined rule in between), strict improvement, iteration cap. Check: MQ is non-decreasing per iteration; the two-clique fixture converges to the two cliques for seeds 0..31; η ∈ {0, 50, 100} reach the same MQ on the fixture; terminates under the cap on a 200-file random MDG
- [ ] 3.4 Simulated annealing (`P(A) = exp(ΔMQ/T)`, `T ← α·T`, draw per probability). Check: with `T(0) = 100, α = 0.99` the fixture still converges; acceptances are counted; with annealing off the count is 0
- [ ] 3.5 Population: seeds `s..s+p−1`, best MQ, lowest seed on ties, canonical relabel 0..k−1. Check: equal (graph, config, seed) → equal `CommunityMap` across two runs; relabelling is by ascending smallest member

## 4. Landscape (Domain) — depends on 3

- [ ] 4.1 `Graphos.Domain.Community.Landscape`: `RunRecord`, `runLandscape graph cfg seed n`, runs sorted by final MQ. Check: n records; seeds `s..s+n−1`; sorted ascending
- [ ] 4.2 Similarity distribution (Zero/Low/Medium/High over |E|) and basin grouping (MQ rounded to 0.05, ≥ 5% of runs). Check: shares sum to 100%; N identical partitions → 100% High; alternating two partitions → the edges that differ land in Medium
- [ ] 4.3 Random-partition baseline (M partitions, min/median/max MQ) and `rnd:N:D` / `bip:N:D` generators. Check: generators emit exactly `round(D·N(N−1)/2)` distinct pairs; `bip` has no intra-half pair; baseline MQ on the two-clique fixture is below the converged MQ for all seeds 0..31

## 5. Orchestration (UseCase) — depends on 3, 4

- [ ] 5.1 `UseCase/Cluster.hs`: `ClusterAlgorithm = Leiden | Bunch BunchConfig`, dispatch, file cap with reported skip and Leiden fallback, symbol push-back, size-cap post-pass, `ClusterQuality` (algorithm, MQ, modularity, intra-edge fraction). Check: `--cluster-algorithm leiden` partition is byte-identical to today's (existing `DeterminismSpec`/`PermutationSpec` still pass); a graph above the cap yields Leiden's partition and a skip entry; every symbol's community equals its file's cluster under Bunch
- [ ] 5.2 `UseCase/ClusterEval.hs`: `landscape` (runs, baseline, Leiden point folded by majority community, optional control MDG). Check: Leiden point present when the graph carries communities, absent otherwise; control MDG path bypasses graph loading

## 6. CLI, config, outputs (Infrastructure) — depends on 5

- [ ] 6.1 Build flags `--cluster-algorithm`, `--seed`, `--bunch-population`, `--bunch-threshold`, `--bunch-sa`, `--bunch-max-iterations`, `--bunch-max-files`, `--bunch-weighted`; config `clustering.algorithm`, `clustering.seed`, `clustering.bunch.*` with CLI precedence. Check: `ParserSpec` cases; config round-trip in `ConfigSpec`; `--bunch-threshold 101` is rejected
- [ ] 6.2 `clustereval landscape` subcommand (`--graph`, `--runs`, `--seed`, `--random-partitions`, `--random-graph`, `--json`, `--out`), `landscape.json` atomic write, markdown summary. Check: JSON keys `runs`, `similarity`, `basins`, `random_baseline`, `leiden`, `config`; file present after a run
- [ ] 6.3 Report lines (`Partition quality: MQ, modularity, intra-edge fraction`, algorithm name) from the same `CommunityMap` the export writes. Check: report MQ equals `modularizationQuality` of the exported `graph.json` communities

## 7. Verification

- [ ] 7.1 `cabal build --flag dev` (-Werror) and `cabal test` green
- [ ] 7.2 Smoke: `graphos clustereval landscape --runs 100` on the Graphos repo (file granularity) and on `rnd:100:0.05`, `rnd:100:0.5`, `bip:100:0.05`; record here the basins, the similarity table, the random baseline and the Leiden point; confirm the real graph's Zero + High exceeds the dense random graph's
- [ ] 7.3 Smoke: η ∈ {0, 50, 100} and annealing on/off on the Graphos repo; record MQ and MQ-evaluation counts here (expected: same MQ band, more evaluations with higher η, fewer with annealing)
- [ ] 7.4 `openspec validate bunch-search-based-modularization --strict`
