## Why

`--cluster-algorithm bunch` (archived change `2026-11-11-bunch-search-based-modularization`)
works on its unit-test fixtures but gives wrong or no results on real
repositories. Running it on solario-core, the 57 repositories of
`dataset-discovery` and five other Solario builds (2026-11-12) needed four
external workarounds. Each one hides a defect in Graphos:

| # | Defect | Measured effect | Workaround used |
|---|---|---|---|
| 1 | The community-size cap post-pass (`--max-community-fraction`, default 5 % of nodes) also applies to Bunch (`bunch-clustering` *File cap* requirement) | On small graphs it cuts every Bunch module down to single files: nestjs-realworld 29 files gave 29 modules (MQ **0**) instead of 9 (MQ 4.20); angular-realworld gave MQ 1.48 instead of 6.36; solario-core `src/cli` gave 61 singletons and MQ 16.66 instead of 38 modules and MQ 18.37 | `--no-community-cap` |
| 2 | The pipeline clusters twice (`pipeline-clustering`). Step 5 re-clusters a graph enriched with `depends_on` / `inferred` edges that were derived from the Step-4 partition, and the Bunch MDG folds every relation except `contains` (`bunch-clustering` *File-level MDG*) | Bunch re-clusters its own output: in solario-core `src/infrastructure` it saw 1,490 inferred `depends_on` against 316 real imports. In whole-repo builds, `documents` edges add doc-to-code links to the file graph | `--edge-density Sparse` + a derived imports-only graph |
| 3 | Above `--bunch-max-files` (default 5,000) Bunch is silently replaced by Leiden, yet the search stops being tractable well below that: ≈ 1 s per search at 164 files, ≈ 25 s at 440, **> 28 min** at 1,778 (killed) | No Bunch result for any real repository above ~600 files | Split into directory sub-trees of ≤ 500 files, one Bunch run each, then a whole-graph refinement pass |
| 4 | `resolveImport` is purely syntactic: it never sees the corpus file set | Extension-less targets (`src/x/foo` for `foo.ts`), root aliases (`@/x`, `~/x`, `$lib/x`), tsconfig `paths` (`app/config`) and JVM fully-qualified names (`io.spring.core.Article`) stay unresolved or become `external:`. In `dataset-discovery`, 5 of the first 10 repositories had **0** file-to-file imports, jhipster lost 224 internal imports, and taxonomy (Next.js, 131 files) had none | A post-build resolver against `git ls-files` |

**The splitting workaround creates its own defect.** A file whose imports all
cross sub-tree boundaries is left alone in its module, because its own run never
sees its neighbours. pc26 (3,929 files) got 1,579 singletons out of 2,363
modules. A global refinement pass (Bunch's own single-file move, over the whole
graph) removed all but one and raised MQ from 149 to 376:

| Project | Files | Modules | Singletons | MQ | Modularity |
|---|---:|---|---|---|---|
| pc26 | 3,929 | 2,363 → 1,515 | 1,579 → 1 | 149.3 → 376.0 | 0.104 → 0.156 |
| console | 1,543 | 796 → 589 | 275 → 0 | 287.2 → 322.2 | 0.260 → 0.341 |
| llm-telemetry | 994 | 566 → 231 | 370 → 0 | 48.8 → 81.5 | 0.171 → 0.318 |
| platformcon | 670 | 476 → 163 | 384 → 1 | 23.4 → 62.6 | 0.146 → 0.366 |

The workarounds live outside Graphos (`graphos-out-bunch-dataset/`,
`dataset-discovery/scripts/c4/`). This change moves the fixes into Graphos so
`graphos . --cluster-algorithm bunch` gives the same result with no extra
step.

## What Changes

- **D1. No size cap on Bunch.** The community-size cap post-pass SHALL NOT
  apply to a Bunch partition. MQ already penalises oversized modules, and the
  cap is a Leiden safeguard.
- **D2. Bunch clusters extracted dependencies only, once.**
  - The Bunch MDG is built from a configurable allowlist of code-dependency
    relations: default `imports`, `calls`, `references`, `extends`,
    `implements`.
  - It never includes `contains`, `documents`, `depends_on`, `inferred` or
    semantic edges.
  - The Step-5 re-cluster reuses the Step-4 Bunch partition: the MDG is
    unchanged by inference, so the search would only repeat itself.
- **D3. Partitioned Bunch with global refinement, instead of a Leiden fallback.**
  - Above `--bunch-max-files-per-run` (new, default 500) the MDG is split into
    directory sub-trees of at most that size. Tiny sibling directories are
    pooled, and a directory that cannot be split is cut alphabetically.
  - Bunch runs per sub-tree with the configured population and seed.
  - A global refinement pass then runs over the whole MDG: Bunch's
    single-file move to a neighbouring module, next-ascent, every import
    counted, repeated until no move raises MQ.
  - The refinement also runs after an unsplit search. It is a no-op there,
    since the search already stops at a local optimum of the same
    neighbourhood.
  - `--bunch-max-files` keeps its meaning as a hard ceiling; above it the
    stage skips to Leiden as today, with a raised default of 50,000.
  - The report names the sub-tree runs and the refinement moves.
- **D4. Corpus-aware import resolution.** After extraction, and before graph
  build, every import target that `resolveImport` left unresolved or
  `external:` is resolved against the set of extracted files:
  - extension-less paths and directory imports (`/index.*`);
  - root aliases `@/`, `~/`, `#/`, `$lib/`;
  - tsconfig/jsconfig `compilerOptions.paths` when the file exists;
  - JVM fully-qualified names, by unique path suffix.

  A target resolves only when exactly one file matches. Otherwise it stays
  external, unchanged. Resolution never reads outside the corpus and never runs
  code.
- `--edge-density sparse` (lowercase) is already handled by the open change
  `cli-enum-flag-vocabulary`; this change depends on nothing from it.

## Capabilities

### Modified Capabilities

- `bunch-clustering`:
  - File-level MDG: relation allowlist (D2).
  - File cap: partitioned search plus refinement, size cap exempted, Leiden
    only above the hard ceiling (D1, D3).
  - New requirement: global refinement pass (D3).
- `pipeline-clustering`: the Step-5 re-cluster reuses a Bunch partition instead
  of re-searching on the inference-enriched graph (D2).
- `import-resolution`: new requirement, corpus-aware resolution of unresolved
  and alias targets (D4).

## Impact

- **Code:**
  - `Domain/Community/Bunch.hs`: MDG relation filter, `splitBySubtree`,
    `refinePartition`.
  - `UseCase/Cluster.hs`: cap exemption, partitioned dispatch.
  - `UseCase/Pipeline/Core.hs`: Step-5 reuse.
  - `Domain/Config/Clustering.hs` and `CLI/Parser.hs`:
    `bunch.relations`, `--bunch-max-files-per-run`.
  - New `Infrastructure/Extract/ImportResolution.hs`, called from the build use
    case.
- **Tests:**
  - `BunchSpec`: relation filter, split, refinement monotonicity,
    determinism, permutation invariance.
  - `ClusterSpec`: no cap on Bunch, Step-5 reuse.
  - `ImportResolutionSpec`: each resolution rule, plus ambiguity left external.
  - Leiden `DeterminismSpec` / `PermutationSpec` unchanged.
- **Lean (optional arm):** refinement preserves a partition, and every accepted
  move strictly increases MQ (extends `Bunch/MQ.lean`, which already has the
  single-move locality lemma `cf_moveTo`).
- **Output:**
  - Bunch partitions of real repositories change: fewer singletons, higher MQ.
  - `imports` edges increase wherever resolution now succeeds; this affects
    Leiden too, since the graph has more real edges.
  - The Leiden partition algorithm itself is untouched.
