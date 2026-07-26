# Act: Batched refinement + incremental merge index

- Stale-snapshot node-loss bug fixed and documented in the `mergeSmallCommunities` haddock.
- Community counts may shift slightly on graphs that hit the old bug (correct behavior); deterministic goldens unaffected.
