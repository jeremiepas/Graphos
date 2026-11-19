## 0. Prerequisites (see proposal *Dependencies*)

- [x] 0.1 `usable-graph-for-code-review` finished and archived (needed by D4 rule 1). Archived 2026-11-18 in #265
- [x] 0.2 `avi-1765` implemented and archived (Java import resolution; its `GrammarsSpec` failures fixed)
- [x] 0.3 `avi-1440` implemented (its TypeScript `GrammarsSpec` failure fixed). Implemented in #88; archived 2026-11-18
- [x] 0.4 `BoundedDeltaCommunitySpec` "small random graphs" property fixed or bounded, so the full suite terminates
- [x] 0.5 `modularity-undirected-view` implemented (validation numbers use the undirected score). Archived 2026-11-19; the recorded Bunch figures come from undirected builds and are unchanged (`src/cli` re-measured)
- [ ] 0.6 `clustering-evaluation-and-selection` reconciled with the merged Bunch requirements (closed as superseded or rebased)

## 1. D1 — No size cap on Bunch

- [x] 1.1 `UseCase/Pipeline/Core.hs`: skip `capCommunitySizes` and `oversizedCommunities` when the algorithm is Bunch. Check: `ClusterSpec` — 29-file nestjs-realworld fixture keeps fewer modules than files and MQ > 0 with `--max-community-fraction 0.05`; Leiden still capped

## 2. D2 — Relation allowlist, Step-5 reuse

- [x] 2.1 `Domain/Config/Clustering.hs`: `clustering.bunch.relations` (default `imports, calls, references, extends, implements`); reject `contains`, `documents`, `depends_on`, `inferred`. Check: `ConfigSpec`
- [x] 2.2 `Domain/Community/Bunch.hs` `foldToFileMDG`: fold only allowlisted relations. Check: `BunchSpec` — `depends_on`/`documents` edges add no MDG pair; existing fold scenarios unchanged
- [x] 2.3 `UseCase/Pipeline/Core.hs`: for Bunch, Step 5 joins the Step-4 partition to the enriched graph (no second search), recomputes cohesion. Check: `ClusterSpec`/pipeline test — Bunch search invoked once, exported partition = Step-4 partition

- [x] 2.4 Report quality of a Bunch partition scored over the MDG relations (`Cluster.bunchPartitionQuality`, `report-consistency` delta); the report line names them. Check: `ClusterSpec` — Step-5 quality on the inference-enriched graph equals the Step-4 quality, and differs from the whole-graph MQ; `ReportSpec` — scope rendered

- [x] 2.5 `Bunch.hs` `mdgFileSet`: MDG files are code files with a declared node — packages, out-of-corpus paths (import-target placeholders: `External`, line-less `Module`) and doc/image/office files are not; `bunchPartitionQuality` scores only edges between MDG files (inferred code↔doc `references` ignored); `Cluster.attachPlaceholderFiles` gives its nodes the module of most of their neighbours. Check: `BunchSpec` (placeholders not files, no pair), `ClusterSpec` (placeholders join their importers, unreferenced one alone, full cover). Measured: solario-core `src/cli` MDG 351 → 161 files, 90 → 1 singletons, 60 s → 4 s

## 3. D3 — Partitioned search, global refinement

- [x] 3.1 `Bunch.hs` `splitBySubtree :: Int -> [FilePath] -> [[FilePath]]` (split by next path component, pool siblings < max/10, alphabetical cut for a single directory). Check: QuickCheck — every chunk ≤ max, chunks partition the input, order-invariant
- [x] 3.2 Partitioned dispatch in `UseCase/Cluster.hs`: induced MDG per sub-tree, configured seed/population, offset module ids; `--bunch-max-files-per-run` (default 500) and `--bunch-max-files` default raised to 50,000. Check: `ClusterSpec` — 3-sub-tree fixture clustered without Leiden fallback; determinism and permutation invariance
- [x] 3.3 `Bunch.hs` `refinePartition`: next-ascent single-file moves to neighbouring modules with the incremental µ/ε update of design D3, ascending path order, gain > 1e-12, ≤ 50 passes. Check: QuickCheck — MQ non-decreasing, result is a partition, idempotent on its own output; unit — a cross-sub-tree singleton is absorbed
- [x] 3.4 Report: `Partition quality (bunch)` line gains `runs N (sizes …), refinement M moves / P passes`. Check: `ReportSpec`
- [x] 3.5 (optional Lean arm) `Bunch/Refine.lean`: an accepted move keeps a partition and strictly increases the exact MQ numerator; builds on `cf_moveTo`
  - `formalization/lean/GraphosFormalization/Bunch/Refine.lean` (v4.14.0, core only): `moveTo_covers` / `moveTo_other` (a move keeps a partition); `Accepted` = exact positive gain over the source and destination clusters, the refinement's test without rounding; `accepted_increases_mq` (via `cf_moveTo` and `fracLt_addLeft`): the exact MQ of the whole partition strictly increases; fixture `misplaced` on `twoTriangles`: the move is accepted, `52/40 → 84/49`. `lake build` clean; `#print axioms` lists only `propext`, `Quot.sound`.

## 4. D4 — Corpus-aware import resolution

- [x] 4.1 New `Infrastructure/Extract/ImportResolution.hs`: rules 1–3 of the spec as built-in resolvers (`path-alias`, `tsconfig-paths`) of the `avi-1766` `resolve:` framework (or with its precedence if it has not landed), uniqueness rule, tsconfig/jsconfig `paths` reader. Check: `ImportResolutionSpec` — one case per spec scenario, ambiguous suffix left external, scoped package untouched, a `resolve:` entry overrides a built-in rule. Done: 13 examples green. `avi-1766` has not landed, so there is no `resolve:` entry to consult yet; the override case moves to 1766, which registers `path-alias` / `tsconfig-paths` as resolver kinds
- [x] 4.2 Wire it between extraction and `buildGraph` in the build use case, after `usable-graph-for-code-review`'s per-file resolution; drop orphaned `External` nodes. Check: integration test on a fixture with `./b`, `@/lib/x`, `$lib/api.js` (Java covered by `avi-1765`). Done in `Pipeline/Core.hs` Step 2 (one INFO line with per-rule counts); `ImportResolutionSpec` runs real tree-sitter extractions of such fixtures. Not wired into `--watch` incremental re-extraction, which sees only the changed files
- [x] 4.3 Existing `import-resolution` scenarios (Haskell canonical ids, TS connectivity, no orphan imports) still pass. Full suite 1,708 examples, 0 failures (BoundedDelta random-graph property skipped, see 0.4)
- [x] 4.4 `Graph/Core.hs` `mergeExtractions`: a declared node (line span) wins over a line-less placeholder of the same id, in either merge order. Check: `GraphSpec`. Before, 27 of 161 `src/cli` files lost their module node to an importer's placeholder
- [x] 4.5 `UseCase/Extract/Core.hs`: the merged-extraction and accumulator refs are updated atomically (`atomicModifyIORef'`). Concurrent `modifyIORef'` from the extraction threads lost updates, dropping whole files: two cached pc26 runs differed on 787 files (129,311 vs 130,679 nodes); now two runs give 136,886 nodes and identical node sets

## 5. Validation on real repositories

- [x] 5.1 `dataset-discovery` (57 repositories, clones at pinned commits): `taxonomy` ≥ 100 files in the import graph; `jhipster-sample-app` +200 internal imports; nestjs-realworld 9 modules, MQ ≈ 4.20
  - Measured on the D4 build vs `e23e44e` (2026-11-18, pinned commits of `manifest.json`, `--no-cluster`), file-to-file `imports` between real module nodes:
    - jhipster-sample-app `6b000b5`: 0 → 548 imports (265 files), 241 resolved through tsconfig `paths` — **met** (+200).
    - taxonomy `298a885`: 0 → 258 imports, **119 files** — **met** (≥ 100). Needed the vendored TSX grammar: with the plain TypeScript grammar 22 JSX/`.mjs` files had no module node (93 files). All 131 TS/JS files now have one.
    - nestjs-realworld `c1c2cc4`, `--cluster-algorithm bunch`: 0 → 65 imports; **9 modules, report MQ 4.30, 0 singletons** — **met** (after 2.5, 4.4; before: 18 modules, 7 singletons).
- [x] 5.2 solario-core `14d86be28` `src/cli`: MQ ≈ 18.37, 0 singletons (among files with a dependency edge)
  - Measured 2026-11-18 (`graphos src/cli --cluster-algorithm bunch`, seed 0): 161 files, 40 modules (workaround 38), **report MQ 18.79**, modularity 0.37, 6 s. One single-file module, `utils/governance-gate.ts`, has no dependency edge inside `src/cli` (all leave the subtree); 0 singletons among connected files. Before 2.5/4.4: 351 MDG files, 112 modules, 90 singletons, 60 s.
- [x] 5.3 pc26 (3,929 files): ≤ 1 % singletons (among files with a dependency edge), MQ ≥ 330 as printed by the report (scored over the Bunch MDG relations between code files), no Leiden fallback; record wall time. First measured on `e23e44e` from a pre-D4 checkpoint (1,092 modules, MQ 332.33). Re-measured 2026-11-18 on a fresh build of solario-core `9a018285e` with D4, 2.5, 4.4, 4.5, seed 0: 17,137 imports resolved, 4,423 MDG code files, 1,419 modules, **report MQ 410.59**, modularity 0.213, 26 runs, 2,920 refinement moves in 12 passes, Bunch ≈ 370 s, no Leiden fallback; 97 single-file modules, of which 1 has a dependency edge (0.07 %), 96 have none
- [x] 5.4 Leiden `DeterminismSpec` / `PermutationSpec` unchanged; full `cabal test` green
  - 2026-11-19 on `main` `0f60c2c` (after #264–#266): full `cabal test` 1,739 examples, 0 failures, no skips; `DeterminismSpec` (INV-DETERMINISTIC-CLUSTER, ARGMAX-TIE-BREAK-CANONICAL, mergeGraphsAndAnalyze determinism) and `PermutationSpec` (AVI-659 / T3, INV-PARTITION) green.

## 6. Close-out

- [x] 6.1 `openspec validate bunch-real-projects --strict`
  - Valid on 2026-11-19 (after the `report-consistency` delta took in the `modularity-undirected-view` scenarios).
- [ ] 6.2 Replace the external workarounds (`graphos-out-bunch-dataset/c4_project.py`, `refine_record.py`, `dataset-discovery/scripts/c4/c4_build.py` resolvers) with plain `graphos … --cluster-algorithm bunch`; archive the change
