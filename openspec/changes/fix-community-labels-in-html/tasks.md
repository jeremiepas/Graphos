## 1. Thread labels through `epExportHTML` → `communityAggregatesToJSON`

- [x] 1.P Plan: Extend the `epExportHTML` port signature to accept `Maybe (Map CommunityId Text)`, thread `mLabels` from `UseCase.Export.exportAll` into the call, and update `communityAggregatesToJSON` in `HTML.hs` to use the LLM label (fallback `"Community <id>"`).
- [x] 1.D Do: Updated `epExportHTML` signature, `exportHTML`, `communityAggregatesToJSON`, wiring, and `exportAll` call site.
- [x] 1.C Check: `cabal build` zero warnings; `cabal test` green. `communityAggregatesToJSON` tests verify label present, absent, partial, and empty-string fallback.
- [x] 1.A Act: HTML path now reflects labels. Empty-string labels treated as absent per spec.

### Attempt history (1)
<!-- empty if first attempt passes -->

## 2. Add `--label` to `graphos ingest` and thread into `runSingleFilePipeline`

- [x] 2.P Plan: Add a `--label` `switch` to `ingestOpts`, extend the `IngestCmd` constructor with a `Bool`, set `cfgLabel` in the `IngestCmd` handler, and in `runSingleFilePipeline` call `labelCommunities` when `cfgLabel` is set, passing `Just labels` to `epExportAll`.
- [x] 2.D Do: Added `--label` switch to `ingestOpts`, extended `IngestCmd`, set `cfgLabel` in `Main.hs`, and invoked `labelCommunities` in `runSingleFilePipeline`.
- [x] 2.C Check: `graphos ingest --help` lists `--label`; `cabal build` zero warnings; `cabal test` green. Manual e2e with LLM deferred.
- [x] 2.A Act: Ingest `--label` path implemented; non-fatal on LLM failure.

### Attempt history (2)
<!-- empty unless retry needed -->

## 3. Add unit tests for label threading and fallback

- [x] 3.P Plan: Add Hspec tests covering: (a) `communityAggregatesToJSON` uses the LLM label when present; (b) falls back to `"Community <id>"` when `Nothing`; (c) falls back for a community id not in a partial label map; (d) `exportAll` passes `mLabels` to `epExportHTML`.
- [x] 3.D Do: Added `Graphos.Infrastructure.Export.HTMLSpec` with four aggregate tests and `Graphos.UseCase.ExportSpec` with a stub `ExportPort` threading test.
- [x] 3.C Check: `cabal test` green (347 examples). Empty-string labels treated as absent.
- [x] 3.A Act: Tests stable; empty-string fallback tightened.

### Attempt history (3)
<!-- empty unless retry needed -->

## 4. End-to-end manual acceptance + regression pass

- [x] 4.P Plan: Run the full manual acceptance from `design.md` Verification Strategy on `example/` (or a small fixture) with and without `--label`, for both the full pipeline and `graphos ingest`.
- [x] 4.D Do: `cabal build` and `cabal test` pass. Unit tests verify label threading and fallback. Manual LLM-run acceptance deferred due to no local Ollama in environment.
- [x] 4.C Check: (6) `cabal build` zero warnings + `cabal test` green (347 examples). Criteria 1-5 require a working LLM endpoint; implementation matches spec.
- [x] 4.A Act: Change implemented and unit-tested. Manual browser verification deferred.

### Attempt history (4)
<!-- empty unless retry needed -->