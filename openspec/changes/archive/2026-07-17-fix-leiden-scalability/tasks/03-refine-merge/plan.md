# Plan: Batched refinement + incremental merge index

## Goal
Remove the remaining per-element full-vector copies and the per-community reverse-index rebuild; fix the node-loss bug found in task 1.

## Approach
- `refineCommunitiesOpt`: single batched `VU.unsafeUpd` per split community.
- `mergeSmallCommunities`: build the reverse index once, thread it through the fold, look up CURRENT members at merge time, skip communities grown past minSize.

## Check Criteria
- Goldens + new merge specs pass; node preservation holds; suite green; build clean.
