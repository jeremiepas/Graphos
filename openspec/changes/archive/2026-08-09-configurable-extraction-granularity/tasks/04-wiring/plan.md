# Plan: Pipeline wiring

## Goal
Resolved granularity reaches the converter on every tree-sitter path (parallel, bounded, incremental).

## Check Criteria
All call sites resolve via `granularityForFile`; other extraction paths unchanged; build + suite clean.
