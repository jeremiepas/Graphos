## ADDED Requirements

### Requirement: Confidence-weighted modularity (weightedModularity, γ=1)

`Graphos.Domain.Community` SHALL export `weightedModularity :: Graph ->
CommunityMap -> Double` computing the auditable confidence-weighted modularity
`Q_w(γ = 1) = Σ_c [σ_in^w(c)/(2m_w) − σ_tot^w(c)²/(2m_w)²]` over the
configuration null model `P_uv = d_u·d_v/(2m_w)`, where `m_w` is the total
confidence mass (`Σ getConfidence(edgeConfidence e)` over `gEdges`), `d_u` the
weighted degree, and `σ_in^w(c)` the internal confidence mass (doubled internal
edge weight). Edge weights SHALL come from `edgeConfidence` (`edgeWeight` is a
separate semantic attribute and SHALL NOT be mixed in). A graph with `m_w = 0`
or an empty partition SHALL yield `0.0` with no divide-by-zero. Under any
uniform confidence `w₀ > 0` on a **simple** undirected graph (one `gEdges`
record per undirected pair), `Q_w` SHALL reduce exactly to the auditable
AVI-533 `modularity` (INV-CONSTR, AV-ARB-1 scope). Grounding doc:
`docs/math-requirements/AVI-535-weighted-modularity-cohesion.md` §1.4–§1.5, §2.2, §3.

#### Scenario: m_w = 0 yields 0 without divide-by-zero (INV-Qw0)

- **WHEN** `weightedModularity` is called on a graph whose edges all carry confidence `0.0`, or whose `gEdges` is empty
- **THEN** the result is exactly `0.0` (no NaN, no exception)

#### Scenario: reduction to auditable Q under uniform confidence (INV-CONSTR)

- **WHEN** a simple undirected graph has every edge at a common confidence `w₀ ∈ {0.25, 1.0, 4.0}` and a fixed partition
- **THEN** `weightedModularity g cm` equals the AVI-533 `modularity g cm` (within `1e-9`) for each `w₀`

#### Scenario: single community with no dangling edges yields 0 (INV-Qw4)

- **WHEN** every node is in one community (`cm = Map.fromList [(0, allNodes)]`) on a connected undirected graph
- **THEN** `weightedModularity g cm == 0`

#### Scenario: all-singletons partition yields negative Q_w with exact value (INV-Qw4)

- **WHEN** the partition is all singletons on the path `a–b–c` (m = 2, degrees 1,2,1)
- **THEN** the result is negative and equals `−(1/(2m_w))²·Σ d_v²` (`−6/16` for uniform confidence 1, within `1e-9`)

#### Scenario: range bound on undirected fixtures (INV-Qw1)

- **WHEN** `weightedModularity` is evaluated on undirected fixtures (two cliques + bridge; path all-singletons; triangle split)
- **THEN** every result lies in `[-0.5, 1]`

#### Scenario: order independence of the partition (INV-Qw2/Qw3)

- **WHEN** the same partition is given with permuted member lists or permuted `CommunityMap` keys, or the call is repeated
- **THEN** `weightedModularity` returns the same value every time (deterministic, partition-only)

#### Scenario: signal strength is exercised

- **WHEN** two graphs identical in support differ only in the confidence of an inter-community bridge (0.1 vs 10.0)
- **THEN** `weightedModularity` differs between the two for a fixed partition

#### Scenario: INV-CONSTR scoped to simple graphs — bidirectional pair characterization

- **WHEN** an undirected graph stores a bidirectional pair as two `gEdges` records (`a→b`, `b→a`) with a single community `{a, b}` at uniform confidence 1
- **THEN** `weightedModularity` is well-defined (`0.0` for the single all-node community) and independent of member enumeration order (characterization of the AV-ARB-1 divergence; INV-CONSTR equality to legacy `modularity` is scoped to simple graphs and is not required on this fixture)

### Requirement: Weighted cohesion (cohesionWeighted)

`Graphos.Domain.Community` SHALL export `cohesionWeighted :: Graph -> [NodeId]
-> Double` computing `C(S) = (1/|S|)·Σ_{i∈S} in_w(i)/d_w(i)` where `in_w(i)` is
the confidence weight of `i`'s incident edges to other members of `S` and
`d_w(i)` is `i`'s weighted degree. A member with `deg_w(i) = 0` SHALL contribute
`0` (no NaN); a singleton or empty community SHALL be `0`. The fold SHALL run in
sorted member-id order so the result is independent of input order (INV-C3/C4).
Grounding doc: AVI-535 §1.6, §4.

#### Scenario: singleton community yields 0

- **WHEN** `cohesionWeighted g ["a"]` is called on a single-node graph
- **THEN** the result is exactly `0.0`

#### Scenario: deg_w = 0 member contributes 0 (INV-C5)

- **WHEN** a community contains members with no incident edges
- **THEN** each such member contributes `0` and the result is in `[0, 1]` with no NaN

#### Scenario: reduction to spec cohesion under uniform confidence (INV-C2)

- **WHEN** all confidences are uniform (`w₀ ∈ {0.5, 1.0, 4.0}`)
- **THEN** `cohesionWeighted` equals the spec degree-ratio cohesion formula (`cohesionScore` on the same members, within `1e-9` for the pinned fixture: mean `8/9`)

#### Scenario: order independence (INV-C3/C4)

- **WHEN** `cohesionWeighted` is called on `["a","b","c"]` and on `["c","a","b"]`
- **THEN** both calls return the same value

### Requirement: Weighted aggregates are O(N + E)

The weighted modularity/cohesion family (`weightedModularity`,
`cohesionWeighted`, `communityQuality`, `candidateScore`,
`computeCommunityStatsWeighted`, `internalWeight`) SHALL build one shared
weighted-adjacency view (`buildWeightedAdj`: per-source and per-target incident
edge lists + weighted degree from `nodeWeightedDegrees`) costing `O(E)`, so a
whole partition evaluation is `O(N + E)` rather than an `O(E)` rescan of
`gEdges` per community or member. Grounding doc: AVI-535 §6.

#### Scenario: single adjacency pass per entry point

- **WHEN** a weighted-modularity or weighted-cohesion entry point evaluates every community of a partition
- **THEN** each member lookup walks only that member's incident edges (`O(deg)`), so the total cost over the partition is `O(N + E)` (asserted structurally via the shared `buildWeightedAdj` view; no per-community `gEdges` rescan)

### Requirement: Null-model export (NullModel + null_model key)

The system SHALL record which configuration null model is used in the weighted
modularity baseline. `Graphos.Domain.Types.Graph` SHALL define `data NullModel =
DefaultNullModel | DirectedNullModel` (tags `degree_config_undirected` /
`degree_config_directed`) with both `ToJSON` and `FromJSON` instances.
`Graphos.Domain.Analysis.analyze` SHALL default `analysisNullModel` to
`DefaultNullModel` and `Graphos.Infrastructure.Export.JSON` SHALL serialize it
into graph.json as the top-level `null_model` key. Grounding doc: AVI-535 §1.4
(`shall-null-model`), §8 AF-5.

#### Scenario: null_model key present in export

- **WHEN** a clustered graph is exported via `exportGraph`
- **THEN** the JSON contains the top-level `"null_model"` key with value `"degree_config_undirected"` for the default analysis

#### Scenario: NullModel JSON round-trip

- **WHEN** `DefaultNullModel`/`DirectedNullModel` are encoded and decoded
- **THEN** the tags `degree_config_undirected`/`degree_config_directed` round-trip via `ToJSON`/`FromJSON`