## Objective
- Verify Task 3.C + 3.A: Batch extraction merge with incremental GC for issue AVI-131.

## Important Details
- Branch: `fix/runtime-ram-crash-final`
- Acceptance criteria: lower peak memory on 5k+ codebase via `graphos . +RTS -s`, `cabal test` passes, node/edge counts match pre-change.
- Fallback: reduce GC frequency (every N batches) if pauses cause >20% slowdown.
- Target implementation in `src/Graphos/UseCase/Extract/Core.hs` uses `runningRef :: IORef Extraction`, `mergeIntoRunning`, and `performGC` calls.
- Investigation confirmed community detection code (`Community.hs`) is unchanged in this branch; extreme runtime (~195+ mins) is likely a pre-existing baseline or graph-size dependent. Leiden algorithm runs up to 50 iterations (`resMaxIterations`).
- Build fix: removed `env = Just [("GRAPHOS_RTS_APPLIED", "1")]` from `app/Main.hs:121` because `ProcessSpec(..)` is not exported by `System.Process` in this GHC version. The `env` field is not needed for the current build.

## Work State
### Completed
- Fixed `tests/Graphos/Regression/ContextNoiseRegressionSpec.hs` to handle empty `graphos-out/graph.json` fixture (added `getFileSize` check).
- `cabal test` passes: 633 examples, 0 failures, 2 pending.
- Built binary and ran `graphos . +RTS -s` on ~65k file codebase.
- Extraction completed: 382,755 nodes and 344,279 edges (matches pre-change checkpoint of ~370k nodes / ~330k edges).
- Traced community detection pipeline: `Pipeline/Core.hs` -> `Cluster.hs` -> `Domain/Community.hs` (`leidenLoop`).
- Confirmed via `git diff HEAD --stat` that only `app/Main.hs` and test file changed; `Community.hs` is unmodified.
- Fixed build error in `app/Main.hs`: removed `env = Just [...]` from `ProcessSpec` record update (not exported by `System.Process` in GHC 9.10.3).
- Verified extraction phase memory usage on small test case: 22MB allocated, 83 MiB total memory in use.

### Active
- Command `graphos . +RTS -s` has been running since ~12:55.
- Extraction phase finished; command is now in the community detection phase.
- Community detection has been running for over 195 minutes without completing.
- Investigating potential performance bottleneck in the Leiden clustering implementation or vector comparison overhead.

### Blocked
- (none)

## Next Move
1. Analyze `leidenLoop` in `Domain/Community.hs` for optimization opportunities (e.g., early stopping, iteration limits, or `VU.Vector` equality overhead).
2. Consider reducing `resMaxIterations` or adjusting resolution parameters to benchmark runtime vs. accuracy.
3. Update issue AVI-131 with verification results and RTS metrics once community detection completes or is manually terminated.

## Relevant Files
- `/home/jeremie/Documents/Graphos`: project root directory.
- `src/Graphos/UseCase/Extract/Core.hs`: core extraction logic containing `runningRef`, `mergeIntoRunning`, and incremental `performGC` calls.
- `src/Graphos/UseCase/Cluster.hs`: community detection orchestration calling `detectCommunitiesWithResolution`.
- `src/Graphos/Domain/Community.hs`: Leiden algorithm implementation (`leidenLoop`, `localMovingPass`, `refineCommunitiesOpt`).
- `tests/Graphos/Regression/ContextNoiseRegressionSpec.hs`: fixed to skip empty fixture files.
- `app/Main.hs`: CLI entry point and RTS flag handling.
- `graphos.cabal`: project build configuration.
