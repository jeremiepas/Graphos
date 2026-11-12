## 1. Cluster-only persists (#74)

- [x] 1.1 Share the graph.json write (`writeClusterOutput`) and community-graph export between the full pipeline and `runClusterOnlyPipeline`
- [x] 1.2 Cluster-only writes graph.json, exports, community_graph.json; keeps the checkpoint
- [x] 1.3 Test: `ClusterOnlySpec` runs the entry point on a saved checkpoint and asserts the outputs

## 2. Clustering usable on code graphs (#75)

- [x] 2.1 `inferFileCoLocationEdges`: one representative per doc file and code file, cap `maxCoLocationFiles` = 25
- [x] 2.2 `inferDeterministicDocEdgesFor`: Sparse drops co-location
- [x] 2.3 Community detection and cohesion on `undirectedView`
- [x] 2.4 `addEdges` merges adjacency with `Map.unionWith Set.union`
- [x] 2.5 Tests: InferDocumentSpec, CommunitySpec (directed = undirected partition), GraphSpec (addEdges)

## 3. Verification

- [x] 3.1 `cabal build --flag dev` (-Werror) and `cabal test` green
- [x] 3.2 Smoke on a small TypeScript corpus: cluster-only writes graph.json with communities; documents edges bounded, 0 under Sparse
