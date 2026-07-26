# Plan: Resolution order + CLI flag

## Goal
Effective granularity resolved CLI → per-extension → global → default, exposed as `--granularity`.

## Approach
Pure `resolveGranularity` in UseCase.Extract; `cfgGranularity :: Maybe Granularity` on PipelineConfig; optparse flag with strict reader; active-level log line; YAML template section.

## Check Criteria
Hspec precedence cases (4); flag parses fine|function|file and rejects others; build + suite clean.
