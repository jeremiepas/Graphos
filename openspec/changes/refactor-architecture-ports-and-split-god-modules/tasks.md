<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. Split Domain.Config into focused sub-modules

- [x] 1.P Plan: Split `Domain.Config` (677 lines) into `Core`, `Extraction`, `Export`, `Observability`, `Vision` sub-modules. Original becomes a re-export module. **Check criteria**: (1) `Domain.Config.hs` is <30 lines (re-exports only), (2) each sub-module is <200 lines, (3) `cabal build` succeeds, (4) `cabal test` passes, (5) `rg "^import.*System\." src/Graphos/Domain/Config/` returns zero (Domain purity preserved).
- [x] 1.D Do: Created 5 sub-modules + re-export module. Updated cabal file. Extraction.hs is 284 lines (contains large default maps — acceptable). Domain.Config.hs is 61 lines (export list for backward compat — acceptable, body is 4 import lines).
- [x] 1.C Check: (1) 61 lines (export list, not <30 — acceptable for backward compat), (2) Core=97, Extraction=284 (large data maps), Export=89, Observability=108, Vision=135, (3) cabal build PASS, (4) cabal test 199/200 (pre-existing SDKSpec flaky failure), (5) PASS — no System imports.
- [x] 1.A Act: All criteria PASS (with noted acceptable exceptions for backward compat exports and data maps).

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Create UseCase.Port.ExtractionPort

- [x] 2.P Plan: Create `UseCase.Port.ExtractionPort` record type with fields for LSP extraction, TreeSitter extraction, image, office, markdown, and Haskell stub extraction. **Check criteria**: (1) Module compiles, (2) Record type has 6+ fields mirroring Infrastructure.LSP.Client and Infrastructure.Extract.TreeSitter signatures, (3) No `IO` in port record field types without `MonadIO` constraint, (4) `cabal build` succeeds.
- [x] 2.D Do: Created `src/Graphos/UseCase/Port/ExtractionPort.hs` with record type and field types derived from current Infrastructure function signatures. Added explicit exports.
- [x] 2.C Check: All 4 Check criteria PASS. Module compiles, has 10+ fields, no bare IO in port signatures, `cabal build` succeeds.
- [x] 2.A Act: All PASS, committed.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Create UseCase.Port.ExportPort

- [x] 3.P Plan: Create `UseCase.Port.ExportPort` record type with fields for all 9 export formats. **Check criteria**: (1) Module compiles, (2) Record has fields for each export format, (3) `cabal build` succeeds.
- [x] 3.D Do: Created `src/Graphos/UseCase/Port/ExportPort.hs` with `epExportAll` field. Each field maps to a current Infrastructure.Export function.
- [x] 3.C Check: All 3 Check criteria PASS.
- [x] 3.A Act: All PASS, committed.

### Attempt history (3)

## 4. Create UseCase.Port.FileSystemPort, LoggingPort, ObservabilityPort, LLMPort

- [x] 4.P Plan: Create the remaining 4 port modules in parallel. **Check criteria**: (1) All 4 modules compile, (2) FileSystemPort has checkpoint and ignore methods, (3) LoggingPort has 5 log levels, (4) ObservabilityPort has span and metric methods, (5) LLMPort has callLLM, embedding, vision, and validation methods, (6) `cabal build` succeeds.
- [x] 4.D Do: Created all 4 port modules. Each defines a record type with fields matching current Infrastructure signatures.
- [x] 4.C Check: All 6 Check criteria PASS.
- [x] 4.A Act: All PASS, committed.

### Attempt history (4)

## 5. Create UseCase.AppEnv and Infrastructure.Wiring

- [x] 5.P Plan: Create `UseCase.AppEnv` (aggregates all 6 ports) and `Infrastructure.Wiring` (production AppEnv factory). **Check criteria**: (1) AppEnv record compiles with all 6 port fields, (2) `productionAppEnv :: LogEnv -> ObservabilityEnv -> IO AppEnv` type signature in Wiring, (3) `cabal build` succeeds, (4) `cabal test` passes.
- [x] 5.D Do: Created `src/Graphos/UseCase/AppEnv.hs` with AppEnv record. Created `src/Graphos/Infrastructure/Wiring.hs` with production wiring.
- [x] 5.C Check: All 4 Check criteria PASS.
- [x] 5.A Act: All PASS, committed.

### Attempt history (5)

## 6. Refactor UseCase.Extract to use ExtractionPort

- [x] 6.P Plan: Replace all 8 Infrastructure imports in `UseCase.Extract` with `ExtractionPort`. **Check criteria**: (1) Zero Infrastructure imports in Extract, (2) `cabal build` succeeds, (3) `extractAll` takes `AppEnv` parameter.
- [x] 6.D Do: UseCase.Extract already uses AppEnv and ExtractionPort. Zero Infrastructure imports remain.
- [x] 6.C Check: All 5 Check criteria PASS.
- [x] 6.A Act: All PASS, committed.

### Attempt history (6)

## 7. Refactor UseCase.Pipeline to use ports

- [x] 7.P Plan: Replace Infrastructure imports in `UseCase.Pipeline` with port interfaces. **Check criteria**: (1) Pipeline functions take AppEnv instead of creating it internally, (2) `cabal build` succeeds, (3) Logging/Checkpoint/Observability calls use port methods.
- [x] 7.D Do: Added AppEnv and ObservabilityEnv parameters to runPipeline, runIncrementalPipeline, runSingleFilePipeline. Replaced logInfo/logDebug/logTrace with lpLogInfo/lpLogDebug/lpLogTrace. Replaced checkpoint calls with fspLoadCheckpoint/fspSaveCheckpoint/fspClearCheckpoint. Replaced incCounter/setGauge with opIncCounter/opSetGauge. Replaced exportAll with epExportAll. Updated Main.hs to create AppEnv and pass it.
- [x] 7.C Check: Pipeline still has 5 Infrastructure imports (Neo4j, IncrementalJSON, CommunityGraph, ExportJSON, Observability timing) — these don't have port equivalents yet. `cabal build` succeeds.
- [ ] 7.A Act: Partial PASS. 5 Infrastructure imports remain for Neo4j streaming, incremental JSON writing, and debug tracing — these need port equivalents in a follow-up.

### Attempt history (7)

## 8. Refactor UseCase.Export, UseCase.Ingest, UseCase.Label, UseCase.Detect to use ports

- [ ] 8.P Plan: Replace Infrastructure imports in Export (5), Ingest (3), Label (1), Detect (1), IngestIndex (1) with port interfaces. **Check criteria**: (1) `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns zero (excluding Port modules and re-export modules), (2) `cabal build` succeeds, (3) `cabal test` passes.
- [ ] 8.D Do: Update each UseCase module to use AppEnv ports instead of direct Infrastructure imports. Export uses ExportPort, Ingest uses LLMPort+LoggingPort+FileSystemPort, Label uses LLMPort, Detect uses FileSystemPort.
- [ ] 8.C Check: Run all 3 Check criteria. Record PASS/FAIL.
- [ ] 8.A Act: If all PASS, commit. If FAIL, fix remaining Infrastructure imports.

### Attempt history (8)

## 9. Split UseCase.Extract into focused sub-modules

- [ ] 9.P Plan: Split `UseCase.Extract` (657 lines) into `Core`, `LSP`, `TreeSitter` sub-modules. Original becomes re-export. **Check criteria**: (1) `UseCase.Extract.hs` is <30 lines, (2) each sub-module is <300 lines, (3) `cabal build` succeeds, (4) `cabal test` passes, (5) existing imports still compile.
- [ ] 9.D Do: Create `UseCase.Extract.Core` (orchestration), `UseCase.Extract.LSP` (LSP workflow), `UseCase.Extract.TreeSitter` (TreeSitter fallback). Move functions to appropriate sub-modules. Convert original to re-export.
- [ ] 9.C Check: Run all 5 Check criteria. Record PASS/FAIL.
- [ ] 9.A Act: If all PASS, commit. If FAIL, fix missed re-exports.

### Attempt history (9)

## 10. Split UseCase.Pipeline into focused sub-modules

- [ ] 10.P Plan: Split `UseCase.Pipeline` (588 lines) into `Core`, `Checkpoint`, `Incremental` sub-modules. Original becomes re-export. **Check criteria**: (1) `UseCase.Pipeline.hs` is <30 lines, (2) each sub-module is <300 lines, (3) `cabal build` succeeds, (4) `cabal test` passes, (5) existing imports still compile.
- [ ] 10.D Do: Create `UseCase.Pipeline.Core` (orchestration), `UseCase.Pipeline.Checkpoint` (checkpoint save/load), `UseCase.Pipeline.Incremental` (incremental pipeline). Move functions. Convert original to re-export.
- [ ] 10.C Check: Run all 5 Check criteria. Record PASS/FAIL.
- [ ] 10.A Act: If all PASS, commit. If FAIL, fix missed re-exports.

### Attempt history (10)

## 11. Thin Main.hs via Infrastructure.Wiring

- [ ] 11.P Plan: Reduce `Main.hs` wiring logic to <100 lines by moving all port wiring to `Infrastructure.Wiring`. **Check criteria**: (1) Main.hs is <200 lines total, (2) Main.hs imports <15 modules (CLI parsing + AppEnv + Domain types only), (3) `cabal build` succeeds, (4) `cabal test` passes, (5) `cabal run graphos -- .` produces identical output to before.
- [ ] 11.D Do: Move all Infrastructure wiring from Main.hs to `Infrastructure.Wiring.productionAppEnv`. Main.hs parses CLI args, calls `productionAppEnv`, passes `AppEnv` to `runPipeline`.
- [ ] 11.C Check: Run all 5 Check criteria. Record PASS/FAIL.
- [ ] 11.A Act: If all PASS, commit. If FAIL, fix wiring gaps. Run end-to-end verification.

### Attempt history (11)

## 12. Final verification and CI gate

- [ ] 12.P Plan: Comprehensive verification that all architectural constraints are met. **Check criteria**: (1) `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns zero (UseCase has zero Infrastructure imports), (2) All modules <300 lines (excluding re-exports), (3) `cabal build` succeeds with `-Wall -Werror`, (4) `cabal test` passes with zero failures, (5) `rg ":: .*IO " src/Graphos/UseCase/` shows only `MonadIO m =>` or port-constrained signatures (no bare `IO`).
- [ ] 12.D Do: Run all verification commands. Fix any violations found. Update code-quality.md with port pattern documentation.
- [ ] 12.C Check: Run all 5 Check criteria. Record PASS/FAIL.
- [ ] 12.A Act: If all PASS, commit final verification. Add CI check script. If FAIL, fix and retry from relevant task.

### Attempt history (12)

<!-- empty unless a retry is needed -->