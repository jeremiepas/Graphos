# Plan: Force build/cluster spans + edge-collapse guard

## Goal
Make span durations reflect real computation and warn on implausibly sparse code graphs.

## Approach
- Force graph evaluation (`deepseq`) inside the timed build span.
- Force `(commMap, cohesion)` inside the timed cluster span.
- Add `edgeCollapseThreshold` constant and a warning after the build step when the input is code-dominant and `edges/nodes` is below threshold.

## Check Criteria
- Traced pipeline run reports `span_build` and `span_cluster` ≥ 1 ms.
- `cabal test` and `cabal build` pass.
- Sanity guard has unit coverage or is exercised via a traced run with no false positives.

## Affected Files
- `src/Graphos/UseCase/Pipeline.hs`
