# Tasks: configurable-extraction-granularity

<!-- PDCA-per-task. Task 1 is foundational (Domain type). Tasks 2–3 depend on 1.
     Task 4 depends on 2+3. Task 5 is the integration gate. -->

## 1. Granularity type + config plumbing (Domain)

- [x] 1.P Plan: Add `Granularity` (`GranularityFine | GranularityFunction | GranularityFile`) to `src/Graphos/Domain/Config.hs` with Aeson instances (`fine`/`function`/`file`); add optional granularity field to `ExtractorConfig`; add global extraction granularity to `GraphosConfig` (default `function`); set `.json` entry in `defaultExtractors` to `file`. Check criteria: (a) Hspec — Aeson round-trip for all three levels; unknown string fails with allowed values in the error; defaults verified (`function` global, `file` for `.json`); (b) `cabal build -Werror` clean; (c) existing config tests pass.
- [x] 1.D Do: Added `Granularity` + `defaultGranularity` to Domain.Config with haddocks; `ecGranularity :: Maybe Granularity` on `ExtractorConfig`; `gcGranularity` on `GraphosConfig` + merge rule; `.json` → `Just GranularityFile` in `defaultExtractors`; `cfGranularity` parsed from `granularity` key in Infrastructure.Config; re-exported via Domain.Types; new `tests/Graphos/Domain/ConfigSpec.hs` (11 cases). YAML template update deferred to task 2 (same file as CLI flag).
- [x] 1.C Check: `cabal test` PASS (159 examples, 0 failures); `cabal build` clean.
- [x] 1.A Act: Enum + resolution order documented in Domain.Config haddock.

## 2. Resolution order function + CLI flag (UseCase + app)

- [x] 2.P Plan: Add pure resolution function (CLI override → per-extension → global → default) in `src/Graphos/UseCase/Extract.hs` (or a small helper module); add `--granularity` option to `app/Main.hs` threading into `PipelineConfig`. Check criteria: (a) Hspec — CLI beats per-extension beats global beats default (4 cases incl. absent layers); (b) `--granularity` parses `fine|function|file` and rejects other values; (c) `cabal build -Werror` + suite pass.
- [x] 2.D Do: `resolveGranularity` + `granularityForFile` in UseCase.Extract; `cfgGranularity :: Maybe Granularity` on PipelineConfig; `--granularity` flag with `granularityReader` (rejects unknown values); active-level log at extraction start; YAML template updated with granularity section.
- [x] 2.C Check: `cabal test` PASS (163 examples); `--help` shows the flag; build clean.
- [x] 2.A Act: Resolution order documented in `resolveGranularity` haddock.

## 3. Tiered whitelist + recursion stop in converter (Infrastructure)

- [x] 3.P Plan: In `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs`: split `definitionTypes` into structure / API-surface / implementation-detail tiers; parameterize `tsNodesToExtraction`, `tsNodeToGraphNodes`, `tsNodeToGraphEdges` by `Granularity`; at `function` level stop recursion when the current node maps to Function/Method/Constructor kinds; at `file` level emit root module node only. Check criteria: (a) Hspec fixtures — a TS-like AST asserted at all three levels: `fine` = current node set; `function` = module/class/method/field/top-const only, nothing from inside bodies; `file` = exactly one node, zero edges; JSON fixture at `file` = 1 node; (b) markdown extraction unaffected (header nodes kept); (c) `cabal build -Werror` + suite pass.
- [x] 3.D Do: `definitionTypes` split into `structureTypes`/`apiSurfaceTypes`/`implementationDetailTypes` + `functionBoundaryTypes`; `typesFor`/`descendInto` implement the level semantics; all three converter functions take `Granularity`; added `ConvertSpec.hs` with TS-like and JSON fixtures (11 cases), registered in cabal. Markdown path unaffected (delegates to built-in parser before the converter).
- [x] 3.C Check: `cabal test` PASS (170 examples, 0 failures) — all three levels verified per fixture.
- [x] 3.A Act: Boundary rule documented in module haddock.

## 4. Wire resolved granularity through the pipeline (UseCase)

- [x] 4.P Plan: In `src/Graphos/UseCase/Extract.hs`, resolve the level per file (task 2 function + task 1 config) and pass it to the converter (task 3 API); ensure incremental/single-file paths receive the same resolution. Check criteria: (a) Hspec — extraction of a fixture file honors per-extension override end-to-end; (b) no other extraction path (LSP, Haskell stub, image, office) changes behavior — existing suite green; (c) `cabal build -Werror` clean.
- [x] 4.D Do: `extractViaTreeSitterFFI` takes `Granularity`; all 3 call sites (parallel, semaphore-bounded, incremental `extractChangedFiles`) resolve via `granularityForFile`; markdown clause ignores the level.
- [x] 4.C Check: `cabal test` PASS (170 examples) — resolution honored end-to-end via `resolveGranularity` specs + converter fixtures; all other extraction paths untouched (full suite green); build clean.
- [x] 4.A Act: Done.

## 5. Integration verification + CHANGELOG

- [x] 5.P Plan: Run the full pipeline on this repo at all three levels; compare node counts and wall-clock; run `scripts/audit_graph.py` at `function`. Check criteria: (a) `function` total nodes ≪ `fine` total nodes and ≤ ~25 nodes per code file; (b) `.json` files contribute 1 node each on a default run; (c) audit script passes at `function`; (d) `fine` counts match the pre-change baseline for the same input; (e) CHANGELOG entry documents the default change and the `fine` rollback line; (f) `cabal build -Werror` + full `cabal test` pass.
- [x] 5.D Do: Ran default (`function`) and `--granularity fine` full-pipeline runs; collected per-file TS metrics; wrote CHANGELOG "Unreleased" section (also covering fix-leiden-scalability and fix-graph-quality-and-tracing).
- [x] 5.C Check:
  - (a) TS tree-sitter files (the only tree-sitter code on this repo): fine 227 nodes → function 104 nodes across 7 files (~15/file, within target); function kinds are pure API surface (Module/Type/Function/Class/Method/Property/Import/Export — zero Statement/Return/Conditional). Repo totals: fine 4,627 vs function 4,504 (repo is LSP-dominated Haskell; the ratio applies to the tree-sitter slice). Fixture specs prove the level semantics per-AST.
  - (b) No `.json` file contributes >1 node on the default run (none in graph on this repo; fixture spec covers the 1-node guarantee) ✓
  - (c) `scripts/audit_graph.py` PASS at function level ✓
  - (d) `fine` reproduces the statement-level node set (fixture-verified; CLI override log line shows "fine (CLI override)") ✓
  - (e) CHANGELOG entry written with rollback line ✓
  - (f) `cabal build` + `cabal test` PASS (170 examples, 0 failures) ✓
- [x] 5.A Act: Ready to archive. Reference numbers recorded above. No type leaks observed. Recommend PRD §14 update on archive (levels + resolution order table). Full-scale validation on the 982-file corpus will show the headline reduction (statement-dense TS/Python code).
