# Act: Lazy trace-directory creation

## Standardized
- The "traces directory exists if and only if a trace file was written" invariant is now encoded in `flushDebugTrace`.
- This pattern should be the default for all future buffered file outputs in the observability layer.

## Follow-up
- Add a regression test in CI that asserts `traces/` is absent after a default pipeline run.
