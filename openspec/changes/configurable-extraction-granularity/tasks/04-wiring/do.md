# Do: Pipeline wiring

- `extractViaTreeSitterFFI` signature gains `Granularity`; markdown clause ignores it.
- All 3 call sites in `src/Graphos/UseCase/Extract.hs` (parallel batch, semaphore-bounded, `extractChangedFiles` incremental) now pass `granularityForFile config fp`.
