## Purpose

Bounds the community-level update performed by the incremental pipeline under node/edge churn `Δ = (R, A)` between runs: the graph and community partition are updated by a bounded delta whose result equals a full rebuild on `G_{t+1}`, only the churn-affected communities re-cluster, and the same churn always produces the same deterministic output. This capability constrains graph/partition incrementality only; it does **not** govern the extraction/embedding content-addressed cache or its eviction, which are separate surfaces (`incremental-cache-eviction`, the cache requirements of `02-incremental-pipeline`).

## ADDED Requirements

### Requirement: Incremental update equals a full rebuild (PROP-EQV, Theorem 1)

The incremental pipeline SHALL apply churn `Δ = (R, A)` as an update operator `Update(G, Δ) = G'` whose exported graph equals a full rebuild on the post-churn file set: `keys(gNodes G') = keys(gNodes G_{t+1})` and `keys(gEdges G') = keys(gEdges G_{t+1})`. Edge sets SHALL satisfy `E' ∩ {edges incident to R} = ∅` (no surviving edge touches a removed node), no edge of `E'` SHALL have an endpoint outside `V'` (dangling drop, INV-DANGLE), and the node count SHALL satisfy `|V'| = |V| − |R| + |A|` (INV-NODECOUNT). Feasibility record (AVI-519 §7): the current `runIncrementalPipeline` builds only the changed files' subgraph and does not satisfy this requirement; compliance is achieved by the implementation change (merge with the retained graph via `mergeGraphs`, detach removed nodes' edges via reverse adjacency), not by this spec adoption. (Source: AVI-519 §1.3, §2.1, §4 Thm 1, §7; constrains `UseCase.Pipeline.Incremental.runIncrementalPipeline/runSingleFilePipeline`, `Domain.Graph.Core.mergeGraphs`.) — classification: new-feature

#### Scenario: --update output equals a fresh run
- **WHEN** a churn `Δ` is applied by `--update` and the identical post-churn file set is rebuilt by `--fresh`
- **THEN** the exported `graph.json` has equal node sets, equal edge sets, and therefore equal `computeGraphHash` (INV-EQV-EDGE fixture test)

#### Scenario: Removed nodes detach all incident edges
- **WHEN** a node `r ∈ R` is removed
- **THEN** no edge in `E'` has `r` as endpoint, and `|V'| = |V| − |R| + |A|` holds in the arithmetic property test (INV-REMOVAL, INV-NODECOUNT)

### Requirement: Bounded-delta community update (INV-PARTITION, Lemma 1)

After churn, the community assignment `P'` SHALL be a valid partition of `V'` (cover + pairwise-disjoint members; total member length `= |V'|`), and every added/replacement node SHALL be assigned to exactly one community. Re-clustering SHALL be scoped to the affected communities `C(Δ)` (communities containing or adjacent to churned nodes, computed from `gAdjFwd/gAdjBack` alone); communities not in `C(Δ)` SHALL be carried over verbatim from `P_t`. Re-clustering SHALL terminate within `resMaxIterations` with strictly increasing `Q_γ` on improving moves (Theorem 2). (Source: AVI-519 §1.2/§1.4, Lemma 1, Thm 2; constrains `Domain.Community.detectCommunitiesWithResolution`, `Domain.Graph.Mutation.rebuildAdjacency`.) — classification: new-feature

#### Scenario: Partition validity after churn
- **WHEN** churn `Δ` is applied to a graph whose partition `P_t` is valid
- **THEN** `S.length . concat . Map.elems $ commMap' == |V'|` with no node appearing twice, and every added node is in exactly one member list (INV-PARTITION property; Lemma 1)

#### Scenario: Untouched communities are preserved
- **WHEN** churn touches nodes in communities `C(Δ)` only
- **THEN** for every community `c ∉ C(Δ)`, the member list of `c` in `P'` is identical to that in `P_t` (bounded-delta property)

#### Scenario: Termination is bounded
- **WHEN** re-clustering of `C(Δ)` runs
- **THEN** it stops within `resMaxIterations` iterations or on the consecutive-equal-assignment stopping rule, never unbounded

### Requirement: Affected-scope inference under fan-out caps (INV-FANOUT, INV-DEDUP)

Inferred edges (community bridges, doc/code references, semantic edges) SHALL be updated only where their source-of-truth (generating adjacency or community data) intersects the churn scope; unaffected inferred edges SHALL be reused verbatim from `G_t`. Every inferred-edge batch SHALL respect its named cap (`maxCommunityBridges = 10000`, `maxLabelFanOut = 20`, `maxSemanticFanOut = 50`), and dedup SHALL preserve the documented stable input order with first-occurrence-wins, equal to the reference `nubBy` (log-linear `O(k log k)`, never `O(k²)`). (Source: AVI-519 §2.2, §3 INV-FANOUT/INV-DEDUP, §5; constrains `UseCase.Infer.inferEdges`, `bounded-edge-inference` surfaces.) — classification: new-feature

#### Scenario: Unaffected bridges are reused verbatim
- **WHEN** churn's adjacency intersection misses a distant community pair's generating data
- **THEN** that pair's bridge set in `G'` is byte-identical to that in `G_t`

#### Scenario: Fan-out caps hold
- **WHEN** any inferred-edge batch is produced
- **THEN** counts per batch satisfy each named cap (INV-FANOUT property), and dedup equals the `nubBy` reference (INV-DEDUP property)

### Requirement: Sub-linear complexity and efficient removal (Theorem 3, REQ-REMOVAL-EFFICIENT)

Incremental update SHALL cost `O(k log k + |N(Δ)|·polylog + E_Δ·polylog)` versus the `O(N + E)` full-rebuild reference, and node removal SHALL delete incident edges via reverse adjacency (`gAdjBack`/`gAdjFwd`) in `O(deg(n))` per node through a `deleteNodeIncident`-style helper — a full `O(E)` edge-map partition scan SHALL NOT be used on the incremental path (it remains acceptable only for full rebuilds). Adjacency and `gHash` SHALL be rebuilt incrementally for affected nodes only (`O(|N(Δ)|)`), and the modularity delta SHALL be computed by the one-pass gain scheme over affected communities (no per-candidate rescans; correct directed/weighted class per `gDirected`). (Source: AVI-519 §2.3, §4 Thm 3, §5, Lemma 2; constrains `Domain.Graph.Mutation.deleteEdgesTouching`, `Domain.Community.computeCommunityStats/cohesionScore`, `leiden-scalability` CSR surfaces.) — classification: new-feature

#### Scenario: Removal is O(deg), not O(E)
- **WHEN** a node with degree `d` is removed from a large graph (micro-benchmark fixture)
- **THEN** the removal runtime scales with `d·polylog(N)` and the full edge scan is not invoked on the incremental path

#### Scenario: Modularity delta agrees with recomputation
- **WHEN** `Q(G')` is computed by delta accumulation and by direct recomputation over `E'`
- **THEN** the two agree (INV-MODULARITY property), with the formula class matching `gDirected`

### Requirement: Deterministic same-churn output (INV-HASH-DET, §6 decision points)

The same churn SHALL produce the same `gHash(G')` — a pure function of `(V', E')` — regardless of event batching/update order. The four determinism decision points SHALL be pinned: (1) node merge precedence is fixed by configuration, not `Map` iteration order; (2) dedup consumes a stable, documented input order (first-occurrence-wins); (3) inferred-edge tie-breaking (community-bridge selection, fan-out truncation) is deterministic, e.g. by sorted endpoint pair, independent of watch-mode event arrival; (4) hash inputs feed the same canonical sorted key set (`computeGraphHash` over sorted node ids + edge tuples). Checkpoint resume from any stage SHALL reproduce the same `G'` as an uninterrupted run (INV-CHECKPOINT). (Source: AVI-519 §3 INV-HASH-DET/INV-CHECKPOINT, §6; constrains `Domain.Graph.Core.computeGraphHash`, `02-incremental-pipeline` checkpoint resume.) — classification: new-feature

#### Scenario: Shuffled mutation order yields identical hash
- **WHEN** the same churn is applied as events in different batchings/orderings
- **THEN** `gHash(G')` is identical in all orderings (INV-HASH-DET property)

#### Scenario: Checkpoint resume equivalence
- **WHEN** a run is interrupted and resumed at any stage (Detect → Extract → Build → Cluster → Infer + Analyze + Export)
- **THEN** the resumed run's `G'` equals the uninterrupted run's `G'` (INV-CHECKPOINT property)