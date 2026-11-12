## ADDED Requirements

### Requirement: Domain.Community — modularity on the support pairs

`modularity :: Graph -> CommunityMap -> Double` SHALL compute
`Q = Σ_c [σ_in(c)/(2m) − (σ_tot(c)/(2m))²]` over the support pairs of the
graph: each unordered pair `{u, v}` with `u ≠ v` and both endpoints in
`gNodes`, counted once. `m` is the number of support pairs, `deg(v)` the number
of pairs containing `v`, `σ_tot(c) = Σ_{v∈c} deg(v)` and `σ_in(c)` twice the
number of pairs with both endpoints in `c`. The result SHALL NOT depend on
`gDirected` or on which orientations of a pair `gEdges` stores. It SHALL be
`0.0` when `m = 0`. Leiden partitions SHALL be unaffected. (PRD §5.4; Lean:
`GraphosFormalization.UndirectedScoring.RecordGraph.fixed_range_bound`)

#### Scenario: direction does not change the score
- **WHEN** `modularity` scores the same `CommunityMap` on a graph and on its `undirectedView`
- **THEN** both values are equal

#### Scenario: a reversed duplicate does not change the score
- **WHEN** an edge `v → u` is added to a graph that already holds `u → v`
- **THEN** `modularity` of every partition is unchanged

#### Scenario: directed witness
- **WHEN** the graph holds the arcs `0 → 1` and `2 → 3` with `gDirected = True` and the partition is `{0,1}, {2,3}`
- **THEN** `modularity` returns `0.5` (it returned `1.5` before this change)

#### Scenario: both-orientations witness
- **WHEN** the graph holds `0 → 1`, `1 → 0`, `2 → 3`, `3 → 2` with `gDirected = False` and the partition is `{0,1}, {2,3}`
- **THEN** `modularity` returns `0.5` (it returned `1.5` before this change)

#### Scenario: range
- **WHEN** `modularity` scores any partition covering every node of a graph with at least one support pair
- **THEN** the result lies in `[-0.5, 1]`
