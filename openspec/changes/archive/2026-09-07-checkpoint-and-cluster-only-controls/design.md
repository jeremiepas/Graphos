## Context

The pipeline checkpoints after the Build stage (`graph.checkpoint.json`) for
incremental runs. In practice, checkpoint reuse is implicit and unlogged (a
rebuild reused a stale checkpoint, yielding an identical node count after config
changes), and `--cluster-only` re-ran Extract and Infer (~3 min each) rather than
reusing stored nodes/edges — defeating its purpose.

## Goals / Non-Goals

**Goals:**
- Transparent, controllable checkpoint reuse.
- A cluster-only path that truly starts from the checkpoint.
- Clear failure when cluster-only lacks a checkpoint.

**Non-Goals:**
- Changing the checkpoint file format.
- Cache-invalidation heuristics beyond an explicit `--fresh` (SHA256 incremental
  logic stays as-is).

## Decisions

- **Load nodes+edges from the checkpoint for `--cluster-only`** and enter the
  pipeline at the Cluster stage.
  - *Alternative considered:* keep re-extracting — rejected, this is the reported
    defect.
- **`--fresh` bypasses checkpoint discovery** entirely and forces Extract.
  - *Alternative considered:* rely on deleting files manually — rejected, error
    prone (led to a missing graph.json this session).
- **INFO-level decision log** stating resume-vs-full and checkpoint path/age.
  - *Alternative considered:* debug-only — rejected, users need this by default.
- **Fail fast if cluster-only has no checkpoint**, with a message to run a full
  build first.
  - *Alternative considered:* silently fall back to full build — rejected,
    surprising and slow.

## Risks / Trade-offs

- [Stale checkpoint after code/config changes] → `--fresh` provides the escape;
  log surfaces reuse so users notice.
- [Cluster-only on a checkpoint from a different input] → validate checkpoint
  provenance minimally and warn on mismatch.

## Migration Plan

- Additive flags plus corrected `--cluster-only`; no format change.
- Rollback: omit `--fresh`; prior implicit behavior remains for normal runs.
- Verify with `cabal test` (pipeline entry-point selection) and smoke runs timing
  `--cluster-only` (no extraction) versus `--fresh` (full extraction).
