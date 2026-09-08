## Why

Checkpoint behavior is opaque and `--cluster-only` is not actually cluster-only.
This session saw a rebuild silently reuse a stale `graph.checkpoint.json`
(identical node count after config changes), and `--cluster-only` still re-ran
full extraction and edge inference (~3 minutes each) instead of reusing the
checkpoint's nodes and edges. Users cannot tell whether a run resumed or
re-extracted, and cannot force a clean build.

## What Changes

- Add `--fresh` / `--no-checkpoint` to force full re-extraction, ignoring any
  existing checkpoint.
- Log the checkpoint decision at INFO: `Resuming from checkpoint <path> (age ...)`
  versus `Full extraction (no checkpoint / --fresh)`.
- Make `--cluster-only` **truly cluster-only**: load nodes and edges from the
  checkpoint and re-run only the Cluster (and dependent Analyze/Export) stages,
  skipping Extract and Infer.
- Error clearly if `--cluster-only` is requested without a usable checkpoint.

## Capabilities

### New Capabilities
- `checkpoint-controls`: explicit checkpoint reuse controls, transparent logging,
  and a genuine cluster-only path that skips extraction and inference.

### Modified Capabilities
<!-- Confirm during specs phase whether existing pipeline/CLI specs change; the
     --fresh flag and logging are additive, --cluster-only behavior is corrected. -->

## Impact

- **UseCase/Pipeline + Load**: branch on checkpoint presence/freshness; cluster-only
  entry that reuses stored nodes/edges.
- **CLI**: `--fresh` / `--no-checkpoint`; corrected `--cluster-only` semantics.
- **Logging**: checkpoint decision at INFO.
