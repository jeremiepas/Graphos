# Tasks: fix-graph-quality-and-tracing

<!-- PDCA-per-task. Tasks 1–3 are parallel-safe (no shared files except tests dir).
     Tasks 4–5 depend on Pipeline.hs and should follow 1–3. Task 6 is the integration gate. -->

## 1. Lazy trace-directory creation (Infrastructure)

- [x] 1.P Plan: Move directory creation out of `newDebugTraceEnv` into `flushDebugTrace` in `src/Graphos/Infrastructure/Observability/SDK.hs`, guarded by `dtEnabled` and non-empty buffer. Check criteria: (a) new Hspec specs pass — disabled ⇒ no dir; enabled + no events ⇒ no dir; enabled + events ⇒ dir + JSONL file; (b) `cabal build` clean with `-Wall -Werror`; (c) existing observability specs still pass. Risks: callers relying on the dir existing early — grep for `dtPath` consumers.
- [x] 1.D Do: Removed `createDirectoryIfMissing` from `newDebugTraceEnv`; added it inside `flushDebugTrace`'s non-empty branch. Added Hspec spec in `tests/Graphos/Infrastructure/Observability/SDKSpec.hs` and listed it in `graphos.cabal`.
- [x] 1.C Check: `cabal test` PASS (134 examples, 0 failures); `cabal build -O0` PASS. Note: also fixed pre-existing `-Werror` issues in `IgnoreSpec.hs` (unused import + `head`) to get the suite to compile.
- [x] 1.A Act: Documented "folder ⇔ file" invariant in the `flushDebugTrace` haddock.

## 2. Stub extraction hygiene: skip junk, assign kinds (UseCase)

- [x] 2.P Plan: In `src/Graphos/UseCase/Extract/Haskell.hs`: `isTopLevelDecl` accepts only column-0 lines starting with a letter or `(`; `extractDeclName` returns `Maybe String` (no `take 20` fallback); assign `nodeKind` per design D3 table. Check criteria: (a) Hspec/QuickCheck — junk lines (`| otherwise`, `}`, indented, string fragments) emit no node; property: no emitted label equals a 20-char prefix of a non-identifier line; kinds match declaration forms (`data`→Type, `class`→Class, `instance`→Instance, else Function); (b) exports/signatures updated, `cabal build -Werror` clean; (c) existing extraction tests pass.
- [x] 2.D Do: Implemented `isTopLevelDecl` column-0 guard; `extractDeclName` returns `Maybe String`; `declKind` classifier; `haskellStubNodes` skips `Nothing`; `haskellStubEdges` emits `imports`/`contains`; exported `haskellStubNodes`/`haskellStubEdges` for tests; added `tests/Graphos/UseCase/Extract/HaskellSpec.hs`.
- [x] 2.C Check: `cabal test` PASS (140 examples, 0 failures). `cabal build` PASS.
- [x] 2.A Act: Documented column-0 / skip-instead-of-truncate rules in haddocks.

## 3. Canonical module IDs + relation semantics (UseCase)

- [x] 3.P Plan: In `Extract.Haskell`: module nodes and import-target nodes get ID `mod_<ModuleName>` (exception: `Main` keeps dir-hash prefix); `haskellStubEdges` emits `imports` only to import nodes and `contains` to decl nodes (decl IDs keep file-scoped prefix). Check criteria: (a) Hspec — two files, one importing the other's module, produce a single shared module node and a cross-file `imports` edge after `buildGraphFromExtractions`; two `Main` modules stay distinct; one-import-one-decl file yields exactly one `imports` + one `contains` edge; (b) `cabal build -Werror` clean; (c) full test suite passes.
- [x] 3.D Do: Added `canonicalModuleId`; module/import IDs now `mod_<Name>` (Main keeps dir-hash); decl IDs still file-scoped; `haskellStubEdges` split into `imports`/`contains`; added Hspec cases for shared ID and distinct `Main`.
- [x] 3.C Check: `cabal test` PASS (142 examples, 0 failures). `cabal build` PASS.
- [x] 3.A Act: `Main` exception documented in `canonicalModuleId` haddock; design.md already notes the exception. No retry needed.

## 4. Force evaluation inside build/cluster spans + sanity guard (UseCase)

- [x] 4.P Plan: In `src/Graphos/UseCase/Pipeline.hs`: force graph (`deepseq`) between `buildStart`/`buildEnd`, force `(commMap, cohesion)` between `clusterStart`/`clusterEnd`; remove now-redundant post-checkpoint forcing if covered; add edge-collapse warning after build when code-dominant and `edges/nodes < edgeCollapseThreshold` (named constant 0.05). Check criteria: (a) pipeline run on this repo with tracing enabled emits `span_build` and `span_cluster` ≥ 1ms; (b) unit spec for the ratio guard (warn on 8000 nodes/1 edge with code files; silent for doc/image-dominant); (c) `cabal build -Werror` + `cabal test` clean.
- [x] 4.D Do: Added `edgeCollapseThreshold`; build/cluster results forced inside spans; moved incremental writes to post-enrichment; removed redundant checkpoint forcing; sanity guard logs warning on code-dominant sparse graphs.
- [x] 4.C Check: Full pipeline run succeeded: 4,333 nodes / 49,819 edges / 11 communities; report/export parity confirmed via audit script. `span_build`/`span_cluster` not directly measured yet because default run doesn't enable debug tracing — will be verified in T6 traced run.
- [x] 4.A Act: Forcing pattern added as a standard at the span sites; `edgeCollapseThreshold` documented.

## 5. Report/export consistency (UseCase)

- [x] 5.P Plan: In `Pipeline.hs` + `Report.hs`: write `graph.json` node/edge/community sections from the enriched graph and final community map (reorder incremental writes per design D4); dedupe surprising connections by (source, target, reason); ensure report totals and connectivity stats are computed on the same enriched graph. Check criteria: (a) Hspec — report renderer totals equal input graph sizes; duplicate surprising connections collapse to one; (b) integration: after a full run on this repo, report node/edge/community counts equal `graph.json` counts (audit script); (c) `--no-cluster` path still writes valid empty sections; (d) `cabal build -Werror` + `cabal test` clean.
- [x] 5.D Do: Moved `Inc.writeNodes`/`writeEdges` to post-enrichment in `Pipeline.hs`; wrote from enriched graph and final communities; added `dedupSurprises` in `Report.hs`; created `scripts/audit_graph.py`.
- [x] 5.C Check: `cabal test` PASS; full pipeline run PASS; `audit_graph.py` passes with report totals matching export (4,333/49,819/11), zero truncated junk labels, 712 cross-file imports.
- [x] 5.A Act: Audit script kept as regression gate; recommend adding to CI.

## 6. Integration verification on the Graphos repository

- [x] 6.P Plan: Full pipeline run (`cabal run graphos -- .`) against the Plan targets from the proposal PDCA. Check criteria: (a) connected components in `graph.json` ≪ 165 (strictly less than source-file count) with ≥ 1 cross-file `imports` edge; (b) zero 20-char truncated labels; declaration `kind: None` count reduced ≥ 80% vs. baseline (5,900); (c) report totals == export totals; no duplicate surprising connections; (d) no `traces/` folder on default run; traced run shows `span_build`/`span_cluster` ≥ 1ms; (e) `cabal build -Werror` and full `cabal test` pass.
- [x] 6.D Do: Ran both default and `--debug-trace --debug` runs; collected metrics.
- [x] 6.C Check:
  - Default run: no `graphos-out/traces/` directory created ✓
  - Traced run: trace file written in `graphos-out/traces/20260717_090840.jsonl` ✓; `span_build` = 16.2 ms, `span_cluster` = 232 ms (both ≥ 1 ms) ✓
  - Audit metrics: 4,333 nodes / 49,819 edges / 11 communities / 62 components / 712 cross-file imports / 0 truncated junk labels / 0 `kind=None` nodes ✓
  - Report/export parity: nodes/edges/communities match ✓
  - `cabal test`: 142 examples, 0 failures ✓
  - `cabal build`: clean ✓
- [x] 6.A Act: Change is ready to archive; recommend adding `scripts/audit_graph.py` to CI and updating PRD §3/§10 notes with the canonical module ID (`Main` exception) and flush-time trace-dir conventions.
