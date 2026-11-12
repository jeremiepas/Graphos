## 1. D1 — No size cap on Bunch

- [x] 1.1 `UseCase/Pipeline/Core.hs`: skip `capCommunitySizes` and `oversizedCommunities` when the algorithm is Bunch. Check: `ClusterSpec` — 29-file nestjs-realworld fixture keeps fewer modules than files and MQ > 0 with `--max-community-fraction 0.05`; Leiden still capped

## 2. D2 — Relation allowlist, Step-5 reuse

- [x] 2.1 `Domain/Config/Clustering.hs`: `clustering.bunch.relations` (default `imports, calls, references, extends, implements`); reject `contains`, `documents`, `depends_on`, `inferred`. Check: `ConfigSpec`
- [x] 2.2 `Domain/Community/Bunch.hs` `foldToFileMDG`: fold only allowlisted relations. Check: `BunchSpec` — `depends_on`/`documents` edges add no MDG pair; existing fold scenarios unchanged
- [x] 2.3 `UseCase/Pipeline/Core.hs`: for Bunch, Step 5 joins the Step-4 partition to the enriched graph (no second search), recomputes cohesion. Check: `ClusterSpec`/pipeline test — Bunch search invoked once, exported partition = Step-4 partition

## 3. D3 — Partitioned search, global refinement

- [x] 3.1 `Bunch.hs` `splitBySubtree :: Int -> [FilePath] -> [[FilePath]]` (split by next path component, pool siblings < max/10, alphabetical cut for a single directory). Check: QuickCheck — every chunk ≤ max, chunks partition the input, order-invariant
- [x] 3.2 Partitioned dispatch in `UseCase/Cluster.hs`: induced MDG per sub-tree, configured seed/population, offset module ids; `--bunch-max-files-per-run` (default 500) and `--bunch-max-files` default raised to 50,000. Check: `ClusterSpec` — 3-sub-tree fixture clustered without Leiden fallback; determinism and permutation invariance
- [x] 3.3 `Bunch.hs` `refinePartition`: next-ascent single-file moves to neighbouring modules with the incremental µ/ε update of design D3, ascending path order, gain > 1e-12, ≤ 50 passes. Check: QuickCheck — MQ non-decreasing, result is a partition, idempotent on its own output; unit — a cross-sub-tree singleton is absorbed
- [x] 3.4 Report: `Partition quality (bunch)` line gains `runs N (sizes …), refinement M moves / P passes`. Check: `ReportSpec`
- [ ] 3.5 (optional Lean arm) `Bunch/Refine.lean`: an accepted move keeps a partition and strictly increases the exact MQ numerator; builds on `cf_moveTo`

## 4. D4 — Corpus-aware import resolution

- [ ] 4.1 New `Infrastructure/Extract/ImportResolution.hs`: rules 1–4 of the spec, uniqueness rule, tsconfig/jsconfig `paths` reader. Check: `ImportResolutionSpec` — one case per spec scenario, ambiguous suffix left external, scoped package untouched
- [ ] 4.2 Wire it between extraction and `buildGraph` in the build use case; drop orphaned `External` nodes. Check: integration test on a fixture with `./b`, `@/lib/x`, `$lib/api.js`, a JVM import
- [ ] 4.3 Existing `import-resolution` scenarios (Haskell canonical ids, TS connectivity, no orphan imports) still pass

## 5. Validation on real repositories

- [ ] 5.1 `dataset-discovery` (57 repositories, clones at pinned commits): `taxonomy` ≥ 100 files in the import graph; `jhipster-sample-app` +200 internal imports; nestjs-realworld 9 modules, MQ ≈ 4.20
- [ ] 5.2 solario-core `14d86be28` `src/cli`: MQ ≈ 18.37, 0 singletons
- [ ] 5.3 pc26 (3,929 files): ≤ 1 % singletons, MQ ≥ 370, no Leiden fallback; record wall time
- [ ] 5.4 Leiden `DeterminismSpec` / `PermutationSpec` unchanged; full `cabal test` green

## 6. Close-out

- [ ] 6.1 `openspec validate bunch-real-projects --strict`
- [ ] 6.2 Replace the external workarounds (`graphos-out-bunch-dataset/c4_project.py`, `refine_record.py`, `dataset-discovery/scripts/c4/c4_build.py` resolvers) with plain `graphos … --cluster-algorithm bunch`; archive the change
