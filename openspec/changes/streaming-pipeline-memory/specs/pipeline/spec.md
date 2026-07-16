## MODIFIED Requirements

### Requirement: Pipeline phase transitions release previous phase data

The pipeline SHALL include explicit memory release boundaries between phases. After each phase produces its output and the next phase begins, the previous phase's intermediate data SHALL be evaluated and a major garbage collection SHALL be performed. Specifically:

1. After `buildGraphFromExtractions`: evaluate the Graph fields (`gNodes`, `gEdges`, `gAdjFwd`, `gAdjBack`), then `performGC` to reclaim Extraction Maps
2. After `clusterGraphWithResolution` and `scoreAllCohesion`: the CommunityMap and CohesionMap are the only outputs retained; LeidenState SHALL be eligible for GC
3. After `analyzeGraph` (articulation points, god nodes computed): the CachedFGL SHALL be eligible for GC; only the Analysis record is retained
4. After `inferEdges`: the enriched Graph is evaluated; the original Graph SHALL be eligible for GC

Each boundary SHALL use `Control.DeepSeq (deepseq)` or `Control.Exception (evaluate)` to force the output data, followed by `System.Mem (performGC)`.

#### Scenario: Extraction Maps are reclaimable after Graph build
- **WHEN** the pipeline transitions from the Build phase to the Cluster phase
- **THEN** the Extraction Maps (IORef-accumulated node and edge Maps) SHALL be eligible for garbage collection, and the Graph SHALL be fully evaluated in memory

#### Scenario: LeidenState is reclaimable after clustering
- **WHEN** the pipeline transitions from the Cluster phase to the Analyze phase
- **THEN** the LeidenState (Unboxed Vectors, IntMaps) SHALL be eligible for garbage collection, and only CommunityMap and CohesionMap SHALL be retained

#### Scenario: CachedFGL is reclaimable after analysis
- **WHEN** the pipeline transitions from the Analyze phase to the Export phase
- **THEN** the CachedFGL (Patricia Tree) SHALL be eligible for garbage collection, and only the Analysis record (NodeIds, GodNodes, CommunityMap) SHALL be retained

#### Scenario: Peak memory on large codebase is ≤3× final graph size
- **WHEN** Graphos processes a codebase that produces a 200MB Graph in memory
- **THEN** peak memory usage during the entire pipeline (measured by `+RTS -s`) SHALL NOT exceed 600MB