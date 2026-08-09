# Plan: Lazy trace-directory creation

## Goal
Ensure the debug-trace directory is created only when debug tracing is enabled and at least one event is buffered.

## Approach
- Modify `newDebugTraceEnv` to not create the directory.
- Move `createDirectoryIfMissing` into `flushDebugTrace` behind the non-empty buffer branch.
- Add Hspec coverage for disabled, enabled-but-empty, and enabled-with-events scenarios.

## Check Criteria
- Disabled tracing: no directory created.
- Enabled tracing with zero events: no directory created.
- Enabled tracing with events: directory + one JSONL file exist.
- `cabal test` and `cabal build` pass with `-Wall -Werror`.

## Affected Files
- `src/Graphos/Infrastructure/Observability/SDK.hs`
- `tests/Graphos/Infrastructure/Observability/SDKSpec.hs`
- `graphos.cabal`
- `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs` (pre-existing warnings blocked the suite)
