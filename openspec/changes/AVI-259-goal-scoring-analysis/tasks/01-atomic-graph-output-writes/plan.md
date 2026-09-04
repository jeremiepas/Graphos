# Task: Atomic Graph Output Writes

## Goal

Implement atomic graph output writes to prevent partial/corrupted graph.json files during pipeline execution.

## Score: 13.5 (P0) — Highest priority

## Acceptance Criteria

- [ ] Graph output writes use atomic rename (write to temp, then rename)
- [ ] No partial graph.json files on disk after crashes
- [ ] Incremental writer handles concurrent writes safely
- [ ] Existing tests pass
- [ ] Integration test verifies atomic write under crash simulation

## Dependencies

- None (build first)

## Blocks

- All output-dependent features (12 features)
- checkpoint-and-cluster-only-controls
- jgf-graph-serialization
- openspec-view

## Implementation Plan

1. Review current graph output code in `src/Graphos/Domain/Types/Writer.hs`
2. Implement atomic write using temporary file + rename
3. Add crash simulation test
4. Wire into pipeline export step
5. Update spec if needed

## Verification

- Run `cabal test` to ensure existing tests pass
- Create crash simulation test that kills process during write
- Verify no partial files on disk
