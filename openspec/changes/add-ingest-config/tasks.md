# Tasks: add-ingest-config

## 1 — Domain types: IngestConfig, IngestUrlConfig, IngestCategories

- [ ] 1.P Plan: Create `src/Graphos/Domain/Config/Ingest.hs` with all ingest config types (`IngestConfig`, `IngestUrlConfig`, `IngestCategoryConfig`, `IngestCategories`, `FileEntry`) and `defaultIngestConfig`. Add `gcIngest` field to `GraphosConfig` in `Core.hs`. Re-export from `Domain.Config`. Types must be pure (no IO), derive `Eq`, `Show`, `Generic`, have Aeson `ToJSON`/`FromJSON` instances with `.:?` optional fields and sensible defaults. `defaultIngestConfig.icEmbed = False` (backward compat). `IngestCategoryConfig` uses `Maybe` fields for inheritance.

- [ ] 1.D Do:
  - Create `src/Graphos/Domain/Config/Ingest.hs`
  - Modify `src/Graphos/Domain/Config/Core.hs` — add `gcIngest :: IngestConfig` to `GraphosConfig`, update `defaultGraphosConfig`, update `mergeGraphosConfig`
  - Modify `src/Graphos/Domain/Config.hs` — re-export `IngestConfig`, `IngestUrlConfig`, `IngestCategories`, `IngestCategoryConfig`, `FileEntry`, `defaultIngestConfig`

- [ ] 1.C Check: `cabal build` succeeds. `defaultIngestConfig.icEmbed == False`. All fields have Aeson round-trip. `mergeGraphosConfig` preserves `gcIngest` merge logic.

- [ ] 1.A Act: If Aeson field names don't match YAML conventions (snake_case), adjust `fieldLabelModifier` per existing pattern in `Vision.hs`.

---

## 2 — IngestIndex v2: FileEntry and backward-compatible format

- [ ] 2.P Plan: Extend `IngestIndex` in `src/Graphos/Domain/Types/Ingest.hs` with `iiVersion :: !Int` and `iiFiles :: !(Map FilePath FileEntry)`. Create `FileEntry` type with `feHash :: !Text` and `feIngestedAt :: !Text`. Update `ToJSON`/`FromJSON` instances: v1 format (no `version` key) loads with `iiVersion = 1` and `iiFiles = Map.empty`. v2 format loads fully. Always save as v2. Add `lookupFileHash`, `addFileEntry`, `isFileUpToDate` helper functions.

- [ ] 2.D Do:
  - Modify `src/Graphos/Domain/Types/Ingest.hs` — add `iiVersion`, `iiFiles`, `FileEntry` type, update JSON instances, add helpers
  - Modify `src/Graphos/UseCase/IngestIndex.hs` — update `loadIndex` for v1/v2 compat, update `saveIndex` to always write v2

- [ ] 2.C Check: `cabal test` passes. Existing v1 index.json loads without error (`iiFiles` empty, dedup disabled). v2 index.json round-trips. `isFileUpToDate` returns `True` for matching hash, `False` for different hash, `False` for missing file.

- [ ] 2.A Act: If v1 compat adds complexity, simplify: v1 loads as-is with `iiFiles = Map.empty` (no dedup available, which is correct behavior — you can't dedup what you don't have hashes for).

---

## 3 — Infrastructure: ConfigFile parsing and merge for ingest

- [ ] 3.P Plan: Add `cfIngest :: Maybe IngestConfig` to `ConfigFile` in `src/Graphos/Infrastructure/Config.hs`. Add `ingest` field to `parseJSON`. Thread `cfIngest` through `mergeConfig` and `mergeGraphosConfig`. When `cfIngest` is `Nothing`, fall back to `gcIngest defaults` (or `gcIngest global` for global+project merge). When present, merge category-level `Maybe` fields using `Maybe` union (project overrides global).

- [ ] 3.D Do:
  - Modify `src/Graphos/Infrastructure/Config.hs` — add `cfIngest` to `ConfigFile`, `parseJSON`, `mergeConfig`
  - Modify `src/Graphos/Domain/Config/Core.hs` — update `mergeGraphosConfig` for `gcIngest`

- [ ] 3.C Check: `cabal build` succeeds. Parse a YAML with `ingest:` section → `IngestConfig` with all fields. Parse a YAML without `ingest:` → `defaultIngestConfig`. Global+project merge: project `ingest:` overrides global; absent project falls back to global.

- [ ] 3.A Act: If `mergeObservabilityConfig` pattern doesn't fit `IngestConfig` (because of nested `Maybe` categories), write a dedicated `mergeIngestConfig` function that handles `Maybe` field merging.

---

## 4 — CLI: --no-embed flag and Maybe Bool resolution

- [ ] 4.P Plan: Change `IngestCmd` from `IngestCmd FilePath Bool FilePath` to `IngestCmd FilePath (Maybe Bool) FilePath`. Add `--no-embed` flag to `ingestOpts`. Use `optional (flag' True ... <|> flag' False ...)` pattern for three-state parsing. Update `app/Main.hs` ingest handler to resolve `Maybe Bool` against `icEmbed` from `IngestConfig`.

- [ ] 4.D Do:
  - Modify `src/Graphos/CLI/Parser.hs` — change `IngestCmd`, update `ingestOpts` with `--embed`/`--no-embed`
  - Modify `app/Main.hs` — `IngestCmd filePath embedOverride outputDir` handler: resolve `effectiveEmbed = fromMaybe (icEmbed ingestCfg) embedOverride`

- [ ] 4.C Check: `cabal build` succeeds. `--embed` → `Just True`. `--no-embed` → `Just False`. No flag → `Nothing`. Resolution: `Nothing` uses config, `Just b` overrides config. `renderCommandReference` updated with `--no-embed`.

- [ ] 4.A Act: If optparse-applicative requires a different pattern for mutually exclusive flags, use `(<|>)` with `flag'` as in existing observability pattern.

---

## 5 — UseCase.Ingest: Config-driven embed, merge, dedup, URL settings

- [ ] 5.P Plan: Modify `ingestFile` and `ingest` to read from `IngestConfig` instead of hardcoded values. Add `IngestConfig` parameter (or extract from `PipelineConfig`). Implement: (a) category-level embed resolution via `resolveEmbedForCategory`, (b) category-level granularity resolution via `resolveGranularityForCategory`, (c) SHA256 dedup check using `isFileUpToDate`, (d) merge vs standalone mode via `icMerge`, (e) URL timeout/retry/user-agent from `icUrl`.

- [ ] 5.D Do:
  - Modify `src/Graphos/UseCase/Ingest.hs` — add `IngestConfig` parameter, implement category resolution, dedup, merge, URL config
  - Modify `src/Graphos/UseCase/Pipeline.hs` — thread `IngestConfig` through `runSingleFilePipeline`, override cluster params from `icResolution`/`icMinCommSize`/`icMaxLeidenIter`

- [ ] 5.C Check: `cabal build` succeeds. Dedup: re-ingesting same file with `icDeduplicate=True` skips extraction. Category resolution: `.hs` file with `categories.code.embed: true` and top-level `embed: false` → embeds (category override wins). URL: timeout value from config passed to HTTP client.

- [ ] 5.A Act: If SHA256 computation requires a new dependency, use `Data.Digest.Pure.SHA` from the `cryptohash` package or the built-in `hash` from `bytestring`. Prefer what's already in the dependency tree.

---

## 6 — PipelineConfig: Add cfgIngest field

- [ ] 6.P Plan: Add `cfgIngest :: IngestConfig` to `PipelineConfig` in `src/Graphos/Domain/Types/Pipeline.hs`. Update `defaultConfig` with `cfgIngest = defaultIngestConfig`. This threads ingest config through the pipeline so `UseCase.Ingest` and `UseCase.Pipeline` can access it without separate parameters.

- [ ] 6.D Do:
  - Modify `src/Graphos/Domain/Types/Pipeline.hs` — add `cfgIngest` field, update `defaultConfig`
  - Modify `src/Graphos/CLI/Parser.hs` — no changes needed (ingest config comes from `GraphosConfig`, not CLI)

- [ ] 6.C Check: `cabal build` succeeds. `defaultConfig.cfgIngest == defaultIngestConfig`. All pattern matches on `PipelineConfig` still compile (may need to add `cfgIngest` to existing patterns).

- [ ] 6.A Act: If adding a field to `PipelineConfig` breaks many pattern matches, use the `cfgIngest = defaultIngestConfig` in all pattern positions or use record update syntax.

---

## 7 — Scaffold template: Add ingest section to graphos.yaml

- [ ] 7.P Plan: Update `defaultConfigYaml` in `app/Main.hs` to include the `ingest:` section with codebase-optimized defaults (`embed: true`, `resolution: 0.8`, `min_comm_size: 2`, etc.). Also update `file_extensions.doc` to `[".md"]` only and `file_extensions.paper` to `[".pdf"]` only in the scaffold.

- [ ] 7.D Do:
  - Modify `app/Main.hs` — update `defaultConfigYaml` string with `ingest:` section and updated `file_extensions`

- [ ] 7.C Check: `graphos init` → `graphos.yaml` contains `ingest:` section with `embed: true`. The file parses correctly. `file_extensions.doc` lists only `[".md"]`. `file_extensions.paper` lists only `[".pdf"]`.

- [ ] 7.A Act: If the YAML template gets too long, consider generating it from a data structure instead of hardcoded strings. For now, the string template approach is consistent with existing code.

---

## 8 — Tests: IngestConfig parsing, merge, resolution, index v1/v2

- [ ] 8.P Plan: Write Hspec unit tests for: (a) `IngestConfig` FromJSON/toJSON round-trip, (b) `mergeConfig` and `mergeGraphosConfig` with ingest section, (c) three-state embed resolution (Just True, Just False, Nothing), (d) category-level embed/granularity resolution, (e) v1 index.json backward compat, (f) v2 index.json round-trip, (g) dedup logic (same hash → skip, different hash → re-extract, missing → add).

- [ ] 8.D Do:
  - Create `tests/Graphos/Domain/Config/IngestSpec.hs`
  - Create `tests/Graphos/Domain/Types/IngestSpec.hs`
  - Modify test suite to include new spec modules

- [ ] 8.C Check: `cabal test` passes. All tests green. Coverage: every `IngestConfig` field parsed from YAML, merge logic correct, resolution priority correct, v1/v2 compat correct.

- [ ] 8.A Act: If test module structure doesn't match existing convention, follow the pattern in `tests/` for other config spec files.

---

## 9 — Docs: Update 10-ingest.md with config section

- [ ] 9.P Plan: Update `docs/workflows/10-ingest.md` to document the new `ingest:` config section, config resolution order (defaults → global → project → CLI), `--no-embed` flag, deduplication behavior, merge vs standalone mode, and per-category overrides. Add the config table from this design.

- [ ] 9.D Do:
  - Modify `docs/workflows/10-ingest.md` — add Configuration section, update When to Use table

- [ ] 9.C Check: Doc accurately reflects implementation. Config table matches `IngestConfig` fields. Examples show common configurations.

- [ ] 9.A Act: N/A — documentation is straightforward.
