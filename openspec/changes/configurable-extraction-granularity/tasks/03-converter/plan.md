# Plan: Tiered whitelist + recursion stop

## Goal
Converter emits nodes per granularity level; function bodies opaque at `function`; root-only at `file`.

## Approach
Split `definitionTypes` into structure/API-surface/implementation-detail tiers + `functionBoundaryTypes`; `typesFor` + `descendInto` drive the walk; converter functions take `Granularity`.

## Check Criteria
Fixture assertions at all three levels (TS-like AST + JSON AST); markdown unaffected; build + suite clean.
