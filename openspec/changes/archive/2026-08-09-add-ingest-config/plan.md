# Plan: add-ingest-config

**Change**: add-ingest-config
**Schema**: pdca
**Attempt**: 1
**Status**: in-progress

## Summary

Implement a dedicated `ingest:` configuration section in `graphos.yaml`, a v2 `IngestIndex` with per-file SHA256 deduplication, three-state CLI embed resolution (`--embed` / `--no-embed`), and per-category overrides. Work through the 9 tasks in `tasks.md` as full PDCA micro-cycles.

## Detail

### Scope

This change touches Domain, UseCase, Infrastructure, CLI, app, tests, and docs layers. All new types are pure (Domain layer, no IO). The built-in default keeps `embed: false` for backward compatibility, while `graphos init` generates `embed: true` as a codebase-optimized default.

### Task Order

1. Domain types: `IngestConfig`, `IngestUrlConfig`, `IngestCategories`
2. `IngestIndex` v2 with `FileEntry` and backward-compatible v1 load
3. Config loader and merge for the `ingest:` section
4. CLI `--no-embed` and `Maybe Bool` resolution
5. UseCase.Ingest config-driven behavior (embed, merge, dedup, URL)
6. `PipelineConfig.cfgIngest` field
7. Scaffold `graphos.yaml` template update
8. Unit tests
9. Documentation update

### Check Criteria (for the entire change)

- `cabal build` succeeds with `-Wall -Werror` dev flags.
- `cabal test` passes, including new and existing tests.
- `graphos init` produces a `graphos.yaml` containing an `ingest:` section with `embed: true`.
- `graphos ingest file.hs` with no config file does not embed (backward compat).
- `graphos ingest file.hs --embed` forces embedding.
- `graphos ingest file.hs --no-embed` with `embed: true` in config disables embedding.
- Existing v1 `index.json` loads without error.

### Risks

- Adding a field to `PipelineConfig` may break pattern matches in multiple modules.
- Per-category `Maybe` merging may need a dedicated `mergeIngestConfig` function.
- SHA256 dependency may not be present in the dependency tree.

### Next Step

Begin Task 1 PDCA micro-cycle: create `tasks/01-domain-types/plan.md`.
