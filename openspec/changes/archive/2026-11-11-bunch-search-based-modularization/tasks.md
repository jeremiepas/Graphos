## 0. Lean arm (formalization companion) — done

- [x] 0.1 `formalization/lean/GraphosFormalization/Bunch/MQ.lean`: exact `cfNum/cfDen`, `mqNum/mqDen`, `intra/total`, `moveTo`; theorems `cfDen_pos`, `cf_le_one`, `cf_closed`, `cf_paper_fixture`, `mqDen_pos`, `mq_le_k`, `intra_le_total`, `mqNum_eq_zero_of_noIntra`, `mu_moveTo`, `ext_moveTo`, `cf_moveTo`, `twoTriangles_mq` (84/49), `twoTriangles_intra` (6/7); imported from the root module; README row. Check: `lean GraphosFormalization/Bunch/MQ.lean` exits 0 with no warnings (done, Lean 4.35.0 locally); `lean.yml` `lake build` green under the pinned 4.14.0 (see `VERIFICATION.md`)
- [x] 0.2 `MQSpec` pins the Lean `twoTriangles` fixture: `MQ = 84/49` within 1e-12, each `CF = 6/7` (µ = 3, ext = 1), intra fraction 6/7 (the "Haskell oracle matches the Lean fixture" scenario)

## 1. Modularization quality (Domain) — done

- [x] 1.1 `Graphos.Domain.Community.MQ`: `MQResult` (MQ, per-cluster `ClusterFactor`, intra-edge fraction, total weight, cluster count), `modularizationQuality :: Graph -> CommunityMap -> MQResult`, unit weights, edges in ascending `gEdges` order and clusters in ascending id order. Check (MQSpec, green): CF(1, 3) = 0.4 (paper Fig. 2, subsystem 2); QuickCheck `CF ∈ [0,1]`, `MQ ∈ [0,k]`, intra fraction `∈ [0,1]`, determinism; closed cluster → 1; all-singletons → 0; empty graph → 0
- [x] 1.2 `modularizationQualityWith EdgeWeights` (`edgeWeight` sums). Check (green): doubling every weight leaves MQ unchanged; `UnitWeights` ignores `edgeWeight`

## 2. Random source and file MDG (Domain) — done

- [x] 2.1 `Graphos.Domain.Random`: SplitMix64 (`nextWord64`, `uniformInt`, `uniformDouble`, `split`, `mkGen`). Check (RandomSpec, green): seed-0 reference outputs 16294208416658607535 / 7960286522194355700 / 487617019471545679; `uniformInt n ∈ [0, n)`; `uniformDouble ∈ [0, 1)`; same seed → same stream
- [x] 2.2 `Graphos.Domain.Community.Bunch.foldToFileMDG`: node → `nodeSourceFile`, all relations except `contains`, undirected, self-pairs dropped, unit or summed weights, files indexed in ascending path order, per-file unboxed adjacency parallel to weights. Check (BunchSpec, green): four symbol edges between two files fold to one pair of weight 1 (unit) / 4 (weighted); `contains`-only graph → zero edges; same-file edge → dropped; node-order permutation → identical `FileMDG`

## 3. Bunch search (Domain) — done

- [x] 3.1 `SearchState` in `ST` (`assign`, `mu`, `ext`, sizes, live/free id sets, running MQ); `moveFile` touches only the source and destination clusters (O(deg)); `initState` compresses ids; `randomPartition`. Check (BunchSpec, green): QuickCheck — after every random move on random MDGs (n ≤ 10) the running MQ equals `mqOfAssignment` of the materialised partition within 1e-9
- [x] 3.2 `candidates`: moves to every other live cluster, new singleton (smallest free id), then building blocks over edges ascending; no-op blocks skipped; `applyCandidate` returns ΔMQ and a revert. Check (green): two disjoint edges from one cluster — no single move improves, the block does (MQ 1 → 2)
- [x] 3.3 `iteration` with η (NAHC at 0, SAHC at 100, `⌈η·|C|/100⌉` minimum-examined rule between), strict improvement above 1e-12, `bcMaxIterations` cap. Check (green): MQ trace non-decreasing; two 5-cliques converge to the cliques for seeds 0..31 with `MQ = 2·20/21`; η ∈ {0, 50, 100} reach the same MQ; iterations below the cap
- [x] 3.4 Annealing (`exp(ΔMQ/T)` for ΔMQ < 0, `T ← α·T` per draw, accepted count). Check (green): off by default → 0 acceptances; `T0 = 100, α = 0.99` terminates and reports acceptances
- [x] 3.5 `bunchCluster`: seeds `s..s+p−1`, best MQ, lowest seed on ties; final MQ recomputed over the canonical partition so equal partitions compare bit-equal; clusters relabelled by ascending smallest member. Check (green): reproducible; best = lowest seed among the maximum; node-order permutation → same result
- [x] 3.6 Scale check: terminates under the cap on a 200-file random MDG and record evaluations/iterations here — recorded: `bunchSearch` (default config, seed 0) on `rnd:200:0.05` (200 files, 990 edges): **3,128,933 MQ evaluations, 576 iterations, MQ 11.54, 86 clusters** (test: BunchSpec "scale check", compiled run 0.57 s). Full default population (seeds 0..7) also terminates well under the 10,000-iteration cap: evaluations 2.55M–3.71M, iterations 461–1,346, final MQ 11.34–12.29, clusters 80–89; best = seed 3 (MQ 12.287, 951 iterations)

## 4. Landscape (Domain) — depends on 3

- [x] 4.1 `Graphos.Domain.Community.Landscape`: `RunRecord`, `runLandscape graph cfg seed n`, runs sorted by final MQ. Check: n records; seeds `s..s+n−1`; sorted ascending — LandscapeSpec green (records, seeds 3..22 for seed 3/n 20, sorted, deterministic)
- [x] 4.2 Similarity distribution (Zero/Low/Medium/High over |E|) and basin grouping (MQ rounded to 0.05, ≥ 5% of runs). Check: shares sum to 100%; N identical partitions → 100% High; alternating two partitions → the edges that differ land in Medium — LandscapeSpec green (single all-intra partition → 100% High; alternating cliques/anti-cliques → 20/21 Medium + 1/21 Zero; 60/40 two-basin grouping with MQ and cluster-count ranges)
- [x] 4.3 Random-partition baseline (M partitions, min/median/max MQ) and `rnd:N:D` / `bip:N:D` generators. Check: generators emit exactly `round(D·N(N−1)/2)` distinct pairs; `bip` has no intra-half pair; baseline MQ on the two-clique fixture is below the converged MQ for all seeds 0..31 — LandscapeSpec green (247/2475 counts, seeded equality, no intra-half pairs, rbMax < converged MQ for seeds 0..31). **Spec inconsistency surfaced:** the spec's `bip:100:0.75` scenario demands 3,712 edges with no intra-half pair, but a bipartite graph on 100 nodes (halves 50/50) has at most ⌊N/2⌋·⌈N/2⌉ = 2,500 cross pairs — 3,712 cross edges cannot exist. Implemented: the requested count is `⌊D·N(N−1)/2⌋` capped at the cross-pair space, so `bip:100:0.75` yields the complete bipartite graph (2,500). Spec scenario updated accordingly: `bip:100:0.25` → 1,237, plus a saturation scenario for `bip:100:0.75` → 2,500

## 5. Orchestration (UseCase) — done

- [x] 5.1 `UseCase/Cluster.hs`: `ClusterAlgorithm = Leiden | Bunch BunchConfig`, dispatch, file cap with reported skip and Leiden fallback, symbol push-back, size-cap post-pass, `ClusterQuality` (algorithm, MQ, modularity, intra-edge fraction). Check: `--cluster-algorithm leiden` partition is byte-identical to today's (existing `DeterminismSpec`/`PermutationSpec` still pass); a graph above the cap yields Leiden's partition and a skip entry; every symbol's community equals its file's cluster under Bunch — ClusterSpec green (Leiden byte-identical, push-back, cap skip / at-cap run); `clusterAlgorithmFromConfig` and `partitionQuality` added; `ClusterQuality`/`BunchSkip` moved to `Domain.Types.Analysis` so the report can carry them
- [x] 5.2 `UseCase/ClusterEval.hs`: `landscape` (runs, baseline, Leiden point folded by majority community, optional control MDG). Check: Leiden point present when the graph carries communities, absent otherwise; control MDG path bypasses graph loading — ClusterEvalSpec green. Note: the module as merged did not compile (invalid `\|` string escape, two type errors in `foldCommunitiesToFiles`); fixed here

## 6. CLI, config, outputs (Infrastructure) — done

- [x] 6.1 Build flags `--cluster-algorithm`, `--seed`, `--bunch-population`, `--bunch-threshold`, `--bunch-sa`, `--bunch-max-iterations`, `--bunch-max-files`, `--bunch-weighted`; config `clustering.algorithm`, `clustering.seed`, `clustering.bunch.*` with CLI precedence. Check: `ParserSpec` cases; config round-trip in `ConfigSpec`; `--bunch-threshold 101` is rejected — ParserSpec/ConfigSpec green. Config keys: `clustering.bunch.{population, threshold, annealing: {enabled, t0, alpha}, max_iterations, max_files, weighted}` (camelCase `maxIterations`/`maxFiles` also accepted); `--bunch-sa T0,ALPHA` or `--bunch-sa on` (100,0.99); `validClusteringConfig` range-checks the block
- [x] 6.2 `clustereval landscape` subcommand (`--graph`, `--runs`, `--seed`, `--random-partitions`, `--random-graph`, `--json`, `--out`), `landscape.json` atomic write, markdown summary. Check: JSON keys `runs`, `similarity`, `basins`, `random_baseline`, `leiden`, `config`; file present after a run — ClusterEvalSpec (keys, `leiden` absent without communities) and smoke runs below (`landscape.json` written via `writeFileAtomic`). Also accepts `--bunch-threshold`, `--bunch-sa`, `--bunch-max-iterations`, `--bunch-weighted`
- [x] 6.3 Report lines (`Partition quality: MQ, modularity, intra-edge fraction`, algorithm name) from the same `CommunityMap` the export writes. Check: report MQ equals `modularizationQuality` of the exported `graph.json` communities — the pipeline re-scores `finalComm` (after the size cap, the map joined into `graph.json`) and stores it in `analysisPartitionQuality`; ReportSpec/ClusterSpec green. Rendered as `Partition quality (leiden|bunch): MQ = …, modularity = …, intra-edge fraction = …`, plus a `Bunch skipped: N files exceed the cap of C` line on fallback

## 7. Verification

- [x] 7.1 `cabal build --flag dev` (-Werror) and `cabal test` green — see PR for the run
- [x] 7.2 Smoke (2026-11-11), 100 runs, seed 0, 1,000 random partitions:

  | MDG | Files / edges | Final MQ | Basins (bin: runs) | Zero / Low / Medium / High | Baseline min / med / max |
  |---|---|---|---|---|---|
  | Graphos code (`src/`+`app/`, file granularity) | 170 / 6,006 | 2.466–2.530 | 2.45: 41, 2.50: 59 | 33.0 / 28.7 / 35.3 / 3.0 % | 0.223 / 0.516 / 1.006 |
  | `rnd:100:0.05` | 100 / 247 | 11.27–11.91 | 8 bins 11.40–11.80, 5–20 runs each | 9.7 / 33.6 / 50.2 / 6.5 % | 0 / 0.577 / 1.577 |
  | `rnd:100:0.5` | 100 / 2,475 | 1.562–1.674 | 1.55: 10, 1.60: 79, 1.65: 11 | 0.8 / 59.2 / 40.0 / 0 % | 0.240 / 0.590 / 1.019 |
  | `bip:100:0.05` | 100 / 247 | 10.84–11.16 | 10.85–11.05, 5 bins | 7.3 / 36.4 / 51.8 / 4.5 % | 0 / 0.571 / 1.914 |

  Leiden point (code graph, folded by majority community): MQ 0.412, 149 clusters, intra-edge fraction 0.009 — Leiden's symbol communities do not align with files. Real graph Zero + High = 36.0 % > dense random graph 0.8 % ✔. The whole repository (1,494 files, 1,165 of them Markdown) was not tractable: 2 runs did not finish in 37 min, so the smoke used the code tree. Default build of the code tree with `--cluster-algorithm bunch --seed 1 --bunch-population 2`: 24.6 s, report `Partition quality (bunch): MQ = 30.6017, modularity = 0.3966, intra-edge fraction = 0.2933` (Leiden build: MQ 44.6138, modularity 0.4197)
- [x] 7.3 Smoke on the code graph, 5 runs each (seeds 0..4):

  | η | Annealing | Final MQ min / med / max | Mean MQ evaluations | Mean iterations | Accepted |
  |---|---|---|---|---|---|
  | 0 | off | 2.483 / 2.502 / 2.506 | 4,821,653 | 1,792 | 0 |
  | 50 | off | 2.470 / 2.492 / 2.511 | 2,754,745 | 319 | 0 |
  | 100 | off | 2.481 / 2.498 / 2.505 | 4,351,178 | 307 | 0 |
  | 0 | 100, 0.99 | 2.483 / 2.502 / 2.506 | 4,823,912 | 3,938 | 5,349 |
  | 50 | 100, 0.99 | 2.484 / 2.503 / 2.504 | 10,022,691 | 4,423 | 20,455 |
  | 100 | 100, 0.99 | 2.486 / 2.488 / 2.495 | 11,515,800 | 4,385 | 20,455 |

  Same MQ band everywhere ✔. Evaluations do **not** follow the design's expectation: they are not monotone in η (η = 50 is cheapest; NAHC's many cheap iterations cost about as much as SAHC's few full scans), and annealing increases evaluations instead of reducing them
- [x] 7.4 `openspec validate bunch-search-based-modularization --strict`

## Follow-up (out of scope)

- Bunch cost on large MDGs: ~6 s per run at 170 files / 6,006 edges, but > 18 min per run at 1,494 files. The default `--bunch-max-files 5000` is therefore far above what finishes in practice; lower the cap or make candidate generation cheaper (building blocks over every edge dominate)
