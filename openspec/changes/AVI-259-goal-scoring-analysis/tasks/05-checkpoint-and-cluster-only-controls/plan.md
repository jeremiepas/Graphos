# Task: Checkpoint and Cluster-Only Controls

## Goal

Add checkpoint resume and cluster-only mode to the pipeline for incremental updates.

## Score: 9.14 (P1) — Fifth highest priority

## Acceptance Criteria

- [ ] `--checkpoint` flag saves pipeline state after each phase
- [ ] `--resume` flag resumes from last checkpoint
- [ ] `--cluster-only` flag skips extraction, runs only clustering
- [ ] SHA256 cache diff for incremental file tracking
- [ ] Checkpoint files are atomic (use atomic-graph-output-writes)

## Dependencies

- atomic-graph-output-writes (P0)

## Blocks

- incremental-pipeline spec
- watch-mode spec
- cluster-composition

## Implementation Plan

1. Design checkpoint data structure (JSON format)
2. Implement checkpoint save after each pipeline phase
3. Implement checkpoint resume logic
4. Add `--cluster-only` mode (skip extraction, load existing graph)
5. Wire SHA256 cache diff into ingest
6. Add tests for checkpoint/resume

## Verification

- Run pipeline with `--checkpoint`, verify state saved
- Kill process mid-pipeline, resume with `--resume`
- Run with `--cluster-only`, verify extraction skipped
- Test incremental update with SHA256 diff
