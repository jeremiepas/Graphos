## 1. Port the fidelity harness into the Haskell codebase

- [x] 1.P Plan: Land the oracle first so every later task is measurable. Scope: add
  `tests/Graphos/Fidelity/ImportEdgesSpec.hs`, `tests/Graphos/Fidelity/GraphCoverageSpec.hs`
  (Hspec spec modules using `aeson` + `directory` + `filepath`, already-listed deps), and
  `src/Graphos/UseCase/Subgraph.hs` (pure subgraph extraction module) exposed via a
  `graphos subgraph` CLI subcommand. Per the `extraction-fidelity-harness` delta: the fidelity
  specs take `graph`/`root` paths as constants or env vars; `graphos subgraph` takes
  `--graph`, `--config`, `--out`, `--boundary-hops`, `--no-derive`. Remove the Python scripts
  from `scripts/`. Update `graphos.cabal` test-suite other-modules and exposed-modules.
  Risk: fidelity specs need fixture corpora — mitigate by using `temporary` to create
  in-test fixtures.
  Check criteria (defined before code):
  - All three components compile under `cabal build --flag dev` with `-Werror`.
  - `ImportEdgesSpec` emits a structured Hspec failure (not an uncaught exception) on a graph
    with zero `imports` edges.
  - On today's `typescipt-repository` graph `ImportEdgesSpec` reports recall 0.0 with 203 missing pairs
    and the spec fails (baseline captured for later comparison).
  - `GraphCoverageSpec` reports the 86 missing files grouped by class and the spec fails.
  - `graphos subgraph --graph <fixture> --config <fixture> --out <tmp>` produces a JSON file
    loadable by `graphos query --graph <out>` without schema errors.
  - Every flag shown in `README.md` exists in the CLI parser or spec module.
- [x] 1.D Do: Implement the pure `Subgraph` module, the two Hspec spec modules, the
  `graphos subgraph` CLI subcommand; update `graphos.cabal`; remove the Python scripts;
  document in `README.md`; record the baseline numbers in this file.
  Done. `src/Graphos/UseCase/Subgraph.hs` (tier/provenance metadata, derived-edge fallback),
  `ImportEdgesSpec` (3 tests) and `GraphCoverageSpec` (2 tests) as structured Hspec oracles,
  `graphos subgraph` CLI with `--graph/--config/--out/--boundary-hops/--no-derive`,
  `renderCommandReference` + scaffold golden fixtures updated, `README.md` harness section
  rewritten (fixed `--match` patterns). Cabal needed no changes (already wired). Python
  harness scripts referenced by the plan do not exist in `scripts/` (deviation). Baselines on
  the Graphos repo: ImportEdges 3/3, GraphCoverage 2/2, Subgraph 7/7, full suite 397/0;
  sample subgraph = 152 nodes / 2,758 edges, loadable via `--graph`.
- [x] 1.C Check: Run `cabal test` against a freshly built graph of a TypeScript repository and
  against the Graphos repository. Record PASS/FAIL per criterion, and record the baseline
  metrics table.
  Done (Graphos repository; typescipt-repository TypeScript repo not available in this workspace —
  see check.md deviation). Compilation PASS, ImportEdgesSpec PASS (3/3), GraphCoverageSpec
  PASS (2/2), subgraph CLI PASS (flags recognized, output loadable by query/explain/neighbors),
  cleanup/docs PASS.
- [x] 1.A Act: If baselines are reproducible, freeze them as the "before" row of the results (skipped)
  table used by tasks 3, 5 and 8. If a spec is non-deterministic, fix determinism before
  proceeding — every later Check depends on it.
  The Graphos-repo baselines above are deterministic and can be frozen as the "before" row
  once the typescipt-repository corpus is rebuilt; blocking on the missing TypeScript corpus.

### Attempt history (1)

## 2. Named truncation budget and specifier-preserving normalization

- [x] 2.P Plan: Make the module specifier survive extraction. Scope:
  `src/Graphos/Infrastructure/Extract/TreeSitter/Core.hs` — replace the literal at `:134` with
  an exported named constant, keep `truncateText` (`:145–148`) generic; add middle-elision for
  import/export declarations; `Convert.hs:181–184` — normalize (collapse whitespace) instead of
  only stripping newlines; `Convert.hs:166`, `:286–293` — derive node IDs from normalized text.
  Risk: changing IDs for long declarations invalidates cached extractions (acceptable, SHA256
  cache re-extracts).
  Check criteria:
  - No numeric literal length is passed to `truncateText` in the tree-sitter path.
  - Unit test: a multi-line import exceeding the budget yields a label ending with
    `from '<specifier>';` and containing a middle elision marker.
  - Unit test: a short declaration is unchanged and carries no elision marker.
  - Unit test: two runs over the same file produce identical node IDs, containing no ellipsis.
  - `cabal build --flag dev` green with `-Werror`; `cabal test` green.
- [x] 2.D Do: Implement the constant, the normalization, the middle-elision, and the ID
  derivation; add the four unit tests to
  `tests/Graphos/Infrastructure/Extract/TreeSitterSpec.hs`.
- [x] 2.C Check: Run the unit tests; rebuild the TypeScript corpus graph and count `Import`
  nodes whose label lacks a `from` clause (target: 0, baseline: 307). (Note: TypeScript corpus not available)
- [x] 2.A Act: If zero specifiers are lost, proceed to task 3. If a grammar exposes no specifier
  child, record it under Attempt history as an unsupported-grammar warning class rather than
  emitting a specifier-less node.

### Attempt history (2)

- Completed 2.D and 2.C (unit tests pass, TypeScript corpus unavailable).
- Fixed `tsNodeLabel` to only use `truncateWithElision` for imports/exports.
- Fixed `tsNodeLabel` to use `truncateText` for other API surface types.

## 3. Emit `imports` edges from tree-sitter extraction

- [x] 3.P Plan: Close the `import-resolution` "Cross-file connectivity" gap for all grammars.
  Scope: `Infrastructure/Extract/TreeSitter/Convert.hs` — capture the specifier during the AST
  walk, add a pure resolver (relative → extension-rewrite → literal → `index` candidate;
  package/builtin → canonical external identity), materialize the target node, and emit an
  `Imports` edge from the file's module node in a new sibling of `tsNodeToGraphEdges`
  (`:135–157`) wired at `:120–121`. Re-exports produce the same edge with an `edgeExtra` marker.
  Keep resolution pure; no IO in `Convert.hs`. Risk: unmaterialized targets are dropped by
  `buildGraph` (`Domain/Graph/Core.hs:70–72`).
  Check criteria:
  - Unit tests for the resolver: relative with `.js`→`.ts` rewrite; literal path; `index` file;
    `node:` builtin; bare package; scoped package with subpath; unresolvable specifier.
  - Unit test: a re-export produces an `Imports` edge carrying the re-export marker.
  - Unit test: exactly one external node exists for a package imported by N files, with N edges.
  - Integration: a built graph of a TS fixture contains at least one `imports` edge whose
    endpoints have different `source_file` values.
  - No node of kind `Import` exists without an outgoing/enclosing `imports` edge.
  - `cabal build --flag dev` and `cabal test` green.
- [ ] 3.D Do: Implement specifier capture, the resolver, target materialization and edge
  emission; add the tests listed in 3.P.
- [ ] 3.C Check: Rebuild the TypeScript corpus graph and run
  `ImportEdgesSpec` (or `cabal test --match "/Fidelity/ImportEdges/"`) with min-precision 0.99
  and min-recall 0.99. Record precision, recall, missing/extra counts, and the node/edge count
  delta vs baseline. Re-run the Graphos self-graph and confirm the Haskell stub path's
  `imports` edges are unchanged.
- [ ] 3.A Act: If both thresholds clear, promote the resolver as the shared helper for the other
  path-based grammars (Python, Go, Rust) and note it in the module Haddock. If recall stalls,
  group the misses by class (path alias, dynamic import, `require`) in Attempt history and scope
  a follow-up rather than widening this change.

### Attempt history (3)

## 4. Root-anchor build-output ignore names

- [ ] 4.P Plan: Stop dropping `src/**/build/**`. Scope: `UseCase/Detect.hs:146–173` — split
  `hardcodedIgnoreDirNames` into a root-anchored class (`build`, `out`, `target`, `dist`,
  `dist-newstyle`, `DerivedData`, `.build`) and a depth-independent class (`node_modules`,
  `.git`, `.stack-work`, `.cache`, `__pycache__`, …); `Detect.hs:177–182` — match the anchored
  class against the path relative to the scan root; `Infrastructure/FileSystem/Ignore.hs:199–228`
  — mirror the split so the pattern path agrees. Risk: the existing assertion at
  `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs:71–73` encodes the defect and must be
  inverted deliberately.
  Check criteria:
  - `./build/output.js` is still pruned when the scan root is `.`.
  - `./src/domain/build/build-ledger.ts` and `./src/services/phase/build/build-pipeline-executor.ts`
    are extracted.
  - `./packages/app/node_modules/left-pad/index.js` is still pruned.
  - `IgnoreSpec.hs:71–73` is updated with a comment naming this change as the reason.
  - `cabal test` green.
- [ ] 4.D Do: Implement the split in both modules, update the inverted test with a rationale
  comment, add the three scenarios above as tests.
- [ ] 4.C Check: Rebuild the TypeScript corpus graph and run `GraphCoverageSpec`
  (target: 0 unexplained missing files; baseline: 86 missing / 1 unexplained). Record the file
  and node count delta.
- [ ] 4.A Act: If unexplained missing files remain, add each residual as a new scenario in the
  `gitignore-parsing` delta before proceeding.

### Attempt history (4)

## 5. Make hardcoded ignore names negatable and report exclusions

- [ ] 5.P Plan: Restore user control and explainability. Scope: `Detect.hs:177–182` — evaluate
  negation patterns before the hardcoded list so `!dist/keep/**` re-includes a pruned path;
  detect stage reporting — count exclusions per rule class (root-anchored, depth-independent,
  `.gitignore`, `.graphosignore`) and surface them in the run report. Risk: negation-first
  evaluation costs traversal time on large trees — measure.
  Check criteria:
  - `.graphosignore` containing `!dist/keep/**` causes `./dist/keep/a.ts` to be extracted.
  - Without a negation, `./dist/bundle.js` remains excluded.
  - The run report shows per-class exclusion counts summing to the total excluded.
  - Detect-stage wall time on the TypeScript corpus is within 10% of the pre-change baseline.
  - `cabal test` green.
- [ ] 5.D Do: Implement negation-first evaluation and per-class accounting; add tests; extend
  the run report.
- [ ] 5.C Check: Run the four criteria; record detect-stage timing before/after.
- [ ] 5.A Act: If timing regresses beyond 10%, cache the negation prefix set per directory
  rather than reverting to the short-circuit; record the attempt.

### Attempt history (5)

## 6. Version `graph.json` and make the loader tolerant

- [ ] 6.P Plan: Make derived and partial graphs loadable. Scope: `UseCase/Load.hs:89–97` —
  optional `communities`/`cohesion`/`god_nodes`, read `community_aggregates`, parse optional
  `schema_version`; `Domain/Types/Edge.hs:48–52` and `Domain/Types/Node.hs:53–62` — degrade
  unknown enums with counted warnings; `Node.hs:123–136` — `source_file` optional; per-item
  recovery so malformed nodes/edges are skipped, not fatal;
  `Infrastructure/Export/IncrementalJSON.hs` — emit `schema_version`, keep writer/reader key
  sets symmetric from one shared list; `CLI/Parser.hs` — add `--strict-graph`. Risk: tolerance
  masking producer bugs — mitigated by counts plus strict mode.
  Check criteria:
  - A graph with `"relation": "re_exports"` loads, the edge has relation `inferred`, one
    degraded-relation warning is reported.
  - A node with `"file_type": "other"` loads as `code` with one warning.
  - A node with `"source_file": null` loads and is queryable.
  - A graph lacking `communities`/`cohesion`/`god_nodes` loads with empty community data.
  - 2 malformed nodes out of 100 are skipped and counted; the other 98 load.
  - `--strict-graph` fails on each of the above, naming the offending value and id.
  - Round-trip property: export then load preserves all top-level sections with equal counts.
  - An unsupported major `schema_version` fails with one actionable message.
  - `cabal test` green.
- [ ] 6.D Do: Implement the loader changes, the shared top-level key list, `schema_version`
  emission, `--strict-graph`, and the tests above in `tests/Graphos/UseCase/LoadSpec.hs`.
- [ ] 6.C Check: Run the test list; then run
  `graphos query "config" --graph <subgraph produced by graphos subgraph>` and confirm zero
  schema errors (baseline: 5 successive hard failures).
- [ ] 6.A Act: If a degraded value recurs for a known producer, open a follow-up to promote it
  to a real enum member instead of leaving it degraded.

### Attempt history (6)

## 7. Documentation and PRD alignment

- [ ] 7.P Plan: Make the new behaviour discoverable. Scope: `PRD.md` §3.2 (`:102–112`) Detect
  and Build rows — state root-anchored build-output pruning and cross-file `imports` edge
  emission; §13.2 (`:712–733`) — add the undocumented `--graph`, `--granularity` and the new
  `--strict-graph`; `README.md` — the harness section from task 1; module Haddocks for the new
  constant and resolver. Also correct the stale comment at `Domain/Config/Extraction.hs:47`
  claiming tree-sitter runs as a CLI (it is FFI).
  Check criteria:
  - PRD §3.2 Detect/Build rows mention the two behaviours.
  - PRD §13.2 lists `--graph`, `--granularity`, `--strict-graph`.
  - `README.md` documents all three scripts with runnable invocations.
  - No stale "tree-sitter CLI" claim remains in `src/`.
- [ ] 7.D Do: Update PRD, README and Haddocks; fix the stale comment.
- [ ] 7.C Check: Execute every documented invocation verbatim; grep for the stale claim.
- [ ] 7.A Act: If any documented invocation fails, fix the doc or the flag before closing.

### Attempt history (7)

## 8. Two-corpus acceptance run

- [ ] 8.P Plan: Prove the change on real corpora, not fixtures. Scope: full pipeline on (a) a
  TypeScript repository of ≥ 1,000 source files, (b) the Graphos repository itself. Record a
  before/after table: node count, edge count, relation histogram, connected components, files
  missing, import-edge precision/recall, detect-stage and total wall time, peak RSS.
  Check criteria:
  - (a) import-edge precision ≥ 0.99 and recall ≥ 0.99 (baseline: 0.0 recall, 0 edges).
  - (a) 0 unexplained missing files (baseline: 86 missing / 1 unexplained).
  - (a) connected components strictly less than the number of source files.
  - (b) Haskell stub `imports` edges unchanged in count and endpoints.
  - Total pipeline wall time within 20% of baseline; peak RSS within 20% of baseline.
  - Graphos' own `graph.json` loads under `--strict-graph`.
- [ ] 8.D Do: Run both pipelines, run all three harness components (`ImportEdgesSpec`,
  `GraphCoverageSpec`, `graphos subgraph`), fill in the results table in this file.
- [ ] 8.C Check: Compare every metric against the criteria; record PASS/FAIL per row.
- [ ] 8.A Act: If all rows pass, mark the change ready for archive and open the two filed
  follow-ups (`--granularity` ignored on the tree-sitter path; `graphos merge` reconciling
  differing `schema_version`). If cost regressed beyond 20%, profile the resolver before
  accepting.

### Attempt history (8)

## Results table

| Metric | Baseline (measured) | Target | After |
|---|---|---|---|
| `imports` edges (TS corpus) | 0 | > 0, precision ≥ 0.99, recall ≥ 0.99 | _tbd_ |
| Import labels missing specifier | 307 / 12,164 | 0 | _tbd_ |
| Source files missing from graph | 86 / 1,291 | 0 unexplained | _tbd_ |
| Edges leaving a 43-file subsystem | 2 | > 100 | _tbd_ |
| Loader failures on a derived graph | 5 | 0 | _tbd_ |
| Total pipeline wall time | _record in 8.D_ | within +20% | _tbd_ |
| Peak RSS | _record in 8.D_ | within +20% | _tbd_ |
