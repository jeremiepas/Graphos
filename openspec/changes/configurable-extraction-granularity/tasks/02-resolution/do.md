# Do: Resolution order + CLI flag

- `resolveGranularity` + `granularityForFile` + `granularityName` in `src/Graphos/UseCase/Extract.hs` (exported).
- `cfgGranularity :: Maybe Granularity` added to `PipelineConfig` (+ `defaultConfig`).
- `--granularity LEVEL` option with `granularityReader` in `app/Main.hs` (rejects unknown values with the allowed list).
- Active granularity logged at extraction start, marked "(CLI override)" when the flag is used.
- `graphos.yaml` scaffold template documents the three levels + override syntax.
- 4 resolution-order cases added to `tests/Graphos/UseCase/ExtractSpec.hs`.
