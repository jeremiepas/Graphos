# Plan: Integration + performance verification

## Goal
Prove the fixes meet PRD §16.1 (< 30 s Leiden at 100k nodes) and don't regress the repo-scale case.

## Approach
- Traced pipeline run on this repo → `span_cluster` vs 232 ms baseline.
- Compiled `-O2` benchmark (100k nodes / 120k edges, ring+chords) against both implementations (old via `git stash`).
- Full suite + audit script.

## Check Criteria
- `span_cluster` ≤ 232 ms; 100k synthetic in seconds; suite green; audit passes.
