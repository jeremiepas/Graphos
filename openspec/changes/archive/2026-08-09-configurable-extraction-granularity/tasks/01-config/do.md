# Do: Granularity type + config plumbing

- `src/Graphos/Domain/Config.hs`: `Granularity` enum + Aeson instances (`fine`/`function`/`file`), `defaultGranularity = GranularityFunction`, `ecGranularity` field, `gcGranularity` field + merge rule, `.json` → `Just GranularityFile` in `defaultExtractors`.
- `src/Graphos/Infrastructure/Config.hs`: `cfGranularity` parsed from top-level `granularity:` YAML key, merged in `mergeConfig`.
- `src/Graphos/Domain/Types.hs`: re-exported `Granularity(..)`/`defaultGranularity`.
- `tests/Graphos/Domain/ConfigSpec.hs`: 11 cases (round-trip, rejection, defaults, merge precedence); registered in `graphos.cabal`.
