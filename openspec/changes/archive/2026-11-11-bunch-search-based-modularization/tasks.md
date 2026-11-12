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

## Verification record - PR #248 lane (preserved by the AVI-99 conflict resolution)

Conflict resolution (2026-11-12, [AVI-99](/AVI/issues/AVI-99)): while PR #248 was in
re-review, main already received a finished implementation of tasks 5-7 in #249
(archiving this change). The merge therefore adopts main's implementation -
`ClusterQuality`/`BunchSkip` in `Graphos.Domain.Types.Analysis`, quality carried
via `Analysis.analysisPartitionQuality`, report line
`Partition quality (<algo>): MQ = ..., modularity = ..., intra-edge fraction = ...`
- and PR #248's parallel implementation is not kept in code. The record below
preserves the AVI-65 lane's review and validation history at head `1f27f6c`.
Its implementation-specific details (types in `Graphos.Domain.Community.Quality`,
`exportAll`/`epExportAll` taking the stage's record) describe the
pre-resolution head and are superseded; the semantics they verified - the report
describes the partition the stage actually produced, including Bunch->Leiden
fallback skips - hold equally under main's implementation and are covered by its
`ReportSpec`/`ClusterSpec` cases.

### AVI-99 validation of the merge commit (2026-11-12, head `1b1c9fd`)

- Merge commit `1b1c9fda4e45de49fe0560297f87b50c481df046` (parents: main
  `bb5597a`, PR #248 `1f27f6c`) pushed to `spec/bunch-search-based-modularization`;
  PR #248 `mergeable: true` (remaining state is pending CI only).
- `git diff main..merge -- src app tests graphos.cabal formalization` is empty:
  every source, test and cabal byte matches main; the only delta is this
  documentation file.
- CI on the merge head: `prepare`, `workflow-pinning`, `openspec-archived`
  success; `build` in progress at the time of recording. On main's identical
  tree (`bb5597a`): `build`, `haddock`, `fixture` success; `test` in progress
  (the full suite is a long job). The final green check on the merge head
  belongs to the merge gate (`graphos-keeper`).

### Verification record at head `1f27f6c` (2026-11-11, AVI-65)

Environment: GHC 9.10.3 / cabal 3.16.1.0 in the agent sandbox; the `devenv`
shell is unavailable there. The final run resolved the missing system zlib with
`--constraint="zlib +bundled-c-zlib"` and used `LANG=C.UTF-8` (Haskell's
default ASCII locale cannot read the repo's UTF-8 fixtures).

### Build and tests

- `cabal build --flag dev --constraint="zlib +bundled-c-zlib" all`
  (library + `graphos` executable): **green**, including `-Werror`.
  (System zlib headers are absent in this sandbox; CI/devenv supplies zlib.)
- Full `cabal test --enable-tests --flag dev` with two pre-existing
  exclusions this change does not touch: **1616 examples, 4 failures,
  4 pending** (200 s). The 4 failures are all `GrammarsSpec`
  (PHP/Perl/C# vendored-grammar extraction) — files untouched by this
  change; they were not independently re-run on `main`. The two excluded
  issues are:
  - `Graphos.Domain.Graph.BoundedDeltaCommunitySpec`'s "delta update equals
    full rebuild on small random graphs" property does not terminate (>20 min
    at 100% CPU, also reproduced with `--seed=1`);
  - `GrammarsSpec`'s "TypeScript: only declared types/enum/interface kept"
    fails on `type_identifier` nodes.
  An earlier draft of this record said "1608 examples, 0 failures"; that run
  did not surface the four `GrammarsSpec` cases and conflicted with the PR
  brief's 4-failure figure. This corrected record is the authoritative one
  for head `ab6df99` + the review fix.
- New/affected specs green: `ConfigSpec` (ClusteringConfig defaults, YAML
  decode, JSON round-trip, validation), `ParserSpec` (all eight build flags,
  threshold/algorithm/SA rejection, `clustereval landscape` flags),
  `ReportSpec` (Partition quality line), `ClusterSpec` (Leiden unchanged,
  Bunch push-back, file cap), `ExportSpec` (including the new
  Bunch→Leiden fallback regression below).

### Review fix (AVI-95, changes requested)

- `GRAPH_REPORT.md` now renders the `ClusterQuality` computed by the
  clustering stage instead of re-deriving the algorithm name from config.
  `ClusterQuality`/`BunchSkip` moved to `Graphos.Domain.Community.Quality`;
  `exportAll`/`epExportAll` take the stage's record and the pipeline passes
  `coQuality` on the full and `--cluster-only` paths. On a
  `--bunch-max-files` fallback the line now reads e.g.
  `algorithm=leiden, ..., bunch skipped: 12 files > --bunch-max-files 10
  (ran Leiden fallback)`.
- Regression test: `ExportSpec` "renders the passed quality, including a
  Bunch->Leiden fallback" (config says Bunch; the passed record says Leiden +
  skip; the report must describe what actually ran).

### Deferred non-blocking review observations

The AVI-95 reviewer marked these non-blocking and they are not addressed in
this PR:

- YAML `clustering.algorithm` unknown values are normalised to `leiden` at
  decode time (`Domain/Config/Clustering.hs:104`), so
  `validClusteringConfig`'s unknown-name arm is unreachable from YAML. Decode
  should preserve the raw value and let validation reject it.
- CLI `--bunch-population` / `--bunch-max-iterations` / `--bunch-max-files`
  use bare `option auto`, and CLI overrides are not re-validated
  (`app/Main.hs`); `--bunch-max-files -1` silently always skips.
- Report doubles print at full precision (cosmetic).

### Landscape smoke — Graphos repo

Build: `graphos . -o /tmp/av65-graphos --no-viz --no-observability
--no-semantic-edges --fresh`; 21,662 nodes / 120,134 edges / 1,947
communities. `GRAPH_REPORT.md` contains:
`Partition quality: algorithm=leiden, MQ=407.66865798423396,
modularity=0.271362570841726, intra-edge fraction=0.2543992541661811`.

Landscape: `graphos clustereval landscape --graph /tmp/av65-graphos/graph.json
--runs 100 --bunch-max-iterations 100 --random-partitions 1000` (the iteration
cap is lowered from the 10,000 default so the 100 runs fit the smoke budget;
file MDG = 1,782 files / 35,249 edges):

| MQ bin | Runs | Share | MQ range | Clusters |
|---|---|---|---|---|
| 0.55 | 11 | 11% | 0.5506–0.5903 | 870–1068 |
| 0.60 | 17 | 17% | 0.6049–0.6495 | 1046–1040 |
| 0.65 | 14 | 14% | 0.6508–0.6972 | 903–966 |
| 0.70 | 12 | 12% | 0.7040–0.7457 | 768–1118 |
| 0.75 | 17 | 17% | 0.7626–0.7983 | 402–1047 |
| 0.80 | 6 | 6% | 0.8030–0.8412 | 864–1057 |
| 0.85 | 9 | 9% | 0.8541–0.8998 | 257–902 |

Similarity: Zero 68.86%, Low 30.94%, Medium 0.19%, High 0.01%
(**Zero + High = 68.87%**). Random baseline (1,000 partitions): min 0.0478,
median 0.2312, max 1.0041. Leiden point: MQ 7.7619, 1,164 clusters,
intra-edge fraction 0.0189.

### Landscape smoke — random controls (100 runs each)

| MDG | Edges | Basins (share) | Zero / Low / Medium / High | Z+H | Baseline min/median/max |
|---|---|---|---|---|---|
| `rnd:100:0.05` | 247 | 8 basins, 11.4–11.8 (5–20% each) | 9.72 / 33.60 / 50.20 / 6.48 | 16.19% | 0 / 0.5766 / 1.5770 |
| `rnd:100:0.5` | 2,475 | 1.55 (10%), 1.60 (79%), 1.65 (11%) | 0.81 / 59.23 / 39.96 / 0.00 | **0.81%** | 0.2399 / 0.5901 / 1.0189 |
| `bip:100:0.05` | 247 | 10.85–11.05, 5 basins (6–31% each) | 7.29 / 36.44 / 51.82 / 4.45 | 11.74% | 0 / 0.5713 / 1.9144 |

The real graph's Zero + High (68.87%) exceeds the dense random graph's
(`rnd:100:0.5`, 0.81%), as required. The controls carry no communities, so the
Leiden point is absent from their JSON.

### Threshold / annealing smoke — Graphos repo

`--runs 3 --bunch-max-iterations 10 --random-partitions 100` (same iteration
cap for every row; annealing off rows have 0 accepts):

| Config | Final MQ (3 runs) | MQ evaluations | Annealing accepts |
|---|---|---|---|
| η = 0 | 0.2922 / 0.4220 / 0.4477 | 6,074 / 2,259 / 7,235 | 0 / 0 / 0 |
| η = 50 | 1.0812 / 1.6212 / 3.6683 | 3,116,640 / 4,481,355 / 9,635,695 | 0 / 0 / 0 |
| η = 100 | 4.2613 / 4.4314 / 4.4921 | 19,303,457 / 9,039,144 / 6,313,175 | 0 / 0 / 0 |
| `--bunch-sa 100,0.99` | 0.1612 / 0.3313 / 0.4000 | 41 / 11 / 10 | 5 / 5 / 4 |

At only 10 iterations the threshold changes the achieved MQ (higher η climbs
faster per iteration); the same-band claim is confirmed at convergence on
`rnd:100:0.05` with `--runs 5 --bunch-max-iterations 500`: η = 0 → 10.72–11.81,
η = 50 → 11.40–11.88, η = 100 → 11.35–11.68, with evaluations 304k–465k,
333k–440k and 167k–299k respectively. Annealing on (`T0 = 100, α = 0.99`)
accepts 250–251 worsening moves in 500 iterations and terminates at the cap,
as designed.

### OpenSpec

`openspec validate bunch-search-based-modularization --strict` → valid (exit 0).
