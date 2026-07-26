# Act: Force build/cluster spans + edge-collapse guard

## Standardized
- The forcing pattern (`evaluate` / `deepseq` between start and end timestamps) is now the default for timed pure computations in the pipeline.

## Follow-up
- Add an explicit Hspec unit test for `edgeCollapseThreshold` in a future iteration if the threshold becomes configurable.
