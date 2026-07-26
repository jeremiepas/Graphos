# Do: Force build/cluster spans + edge-collapse guard

## Changes Made
- `src/Graphos/UseCase/Pipeline.hs`
  - Added `edgeCollapseThreshold = 0.05` constant with haddock.
  - Forced graph evaluation inside the build span.
  - Forced `(commMap, cohesion)` inside the cluster span.
  - Added edge-collapse warning after build for code-dominant inputs with low edge/node ratio.
  - Removed redundant post-checkpoint forcing (now covered inside spans).
