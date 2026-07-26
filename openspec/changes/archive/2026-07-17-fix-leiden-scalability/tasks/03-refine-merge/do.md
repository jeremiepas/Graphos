# Do: Batched refinement + incremental merge index

- `refineCommunitiesOpt`: replaced per-node `foldl'` of single-element `unsafeUpd` with one batched `VU.unsafeUpd acc [(i, cid) | i <- wellConnected]` per split community.
- `mergeSmallCommunities`: folds over small community IDs (not stale member snapshots); looks up current members from the evolving map; skips communities that grew ≥ minSize; threads an incrementally-updated reverse index.
- `mergeOne` signature now returns `(CommunityMap, Map NodeId CommunityId)`; `bestNeighborCommunity` takes the maintained index instead of rebuilding it.
- Specs: node-preservation regression case and growth-skip case added to `CommunitySpec.hs`.
