# Plan: Granularity type + config plumbing

## Goal
A three-level `Granularity` enum available at global, per-extension, and (later) CLI scope.

## Approach
- `Granularity` + `defaultGranularity` in `Domain.Config` with `fine`/`function`/`file` Aeson strings.
- `ecGranularity :: Maybe Granularity` on `ExtractorConfig`; `.json` defaults to `file`.
- `gcGranularity :: Granularity` on `GraphosConfig` (default `function`) + merge rule.
- `cfGranularity` in the Infrastructure YAML loader.

## Check Criteria
- Aeson round-trip for all three levels; unknown string rejected with allowed values.
- Defaults verified (`function` global, `file` for `.json`).
- `cabal build -Werror` clean; suite green.
