# bounded-edge-inference Specification

## Purpose
TBD - created by archiving change fix-edge-inference-blowup. Update Purpose after archive.
## Requirements
### Requirement: Community bridges derive from real adjacency

`inferCommunityBridges` (PRD §3 infer stage) MUST emit centroid-to-centroid inferred edges only for community pairs connected by at least one real inter-community edge, and the result MUST be bounded by a named cap constant. It SHALL NOT enumerate all community pairs.

#### Scenario: Adjacent communities are bridged

- **WHEN** two communities are connected by at least one real edge between their members
- **THEN** exactly one inferred centroid bridge is emitted for that pair, verifiable by `cabal test`

#### Scenario: Disconnected communities are not bridged

- **WHEN** two communities share no real inter-community edge
- **THEN** no inferred bridge edge is emitted between them

#### Scenario: Bridge count does not scale quadratically

- **WHEN** a graph produces C communities with far fewer than C² inter-community adjacencies
- **THEN** the number of inferred bridges is at most the number of adjacent community pairs, capped by the named constant

### Requirement: Log-linear edge deduplication

Edge and candidate deduplication in inference and analysis (PRD §16.2 scalability) MUST run in O(k log k), preserving input order and first-occurrence-wins semantics. `nubBy` SHALL NOT be used on lists that scale with graph size.

#### Scenario: Dedup matches nubBy semantics

- **WHEN** a list containing duplicate (source, target) edges in a given order is deduplicated
- **THEN** the output equals the reference `nubBy` result (first occurrence kept, order preserved), verifiable by `cabal test` including a QuickCheck property

### Requirement: Capped doc-code label fan-out

`inferCodeDocEdges` MUST skip labels whose code-node match count exceeds a named fan-out cap; unambiguous labels (matches within the cap) SHALL continue to produce reference edges.

#### Scenario: Ambiguous labels produce no edges

- **WHEN** a doc node's label matches more code nodes than the fan-out cap
- **THEN** no doc-code edges are emitted for that label

#### Scenario: Specific labels still link

- **WHEN** a doc node's label matches a number of code nodes within the cap
- **THEN** reference edges are emitted for those matches, with no duplicate (source, target) pairs

### Requirement: Inference completes within memory bounds at scale

The infer stage MUST complete on 75k-node / 80k-edge graphs without multi-gigabyte memory growth (PRD §16.1, §16.2); inferred edge counts SHALL be proportional to real inter-community adjacency rather than the square of the community count.

#### Scenario: Repository-scale inference stays bounded

- **WHEN** the full pipeline runs at Normal edge density on this repository
- **THEN** the "Inferred N additional edges" count is on the order of the graph's real inter-community adjacency (thousands), not ~C²/2 (previously 48,112 for 314 communities)

### Requirement: Semantic inference fan-out cap

The system SHALL define `maxSemanticFanOut :: Int` (default 50) bounding the number of
`CodeFile` nodes a single `DocFile` node may match via cosine similarity. Doc nodes whose
top-k similar code nodes exceed the cap SHALL only emit edges for the top-`maxSemanticFanOut`
by similarity score. This extends the bounded-edge-inference family alongside the existing
`maxCommunityBridges` (10000) and `maxLabelFanOut` (20) caps.

#### Scenario: Cap respected on high-fan-out doc node

- **WHEN** a doc node has cosine similarity > threshold with 80 code nodes and
  `maxSemanticFanOut = 50`
- **THEN** only the top-50 code nodes by similarity receive `References` edges

#### Scenario: Cap configurable

- **WHEN** `graphos.yaml` sets `semantic_edges.max_fan_out: 100`
- **THEN** `maxSemanticFanOut` is 100 for that run


### Requirement: Edge betweenness scale guards (AVI-534 SG-1/SG-2/SG-3)

Edge betweenness computation SHALL respect the AVI-534 §4 scale guards, grounded in `docs/math-requirements/AVI-534-structural-analysis-complexity.md`:

- **SG-1 (sampled-source cap).** Sampled betweenness draws at most `cfgMaxSampledSources` sources (default 500, CLI `--max-sampled-sources`), in a deterministic seedable order (ascending sequential FGL index, §9). The estimator MUST rescale per-source contributions by `N/|S|` so the cap does not bias the mean (Lemma 3.1).
- **SG-2 (exact all-pairs cap).** Exact O(N·M) all-pairs betweenness MUST NOT be invoked when `N > cfgExactBetweennessNodeCap` (default 10000, CLI `--exact-betweenness-node-cap`); the sampled estimator (SG-1) is used instead. This is a complexity-honesty constraint, not a product decision.
- **SG-3 (per-source memory).** The Brandes forward/backward pass per source MUST allocate O(N + M) working memory over the shared `CachedFGL` and MUST NOT build a fresh structure per source beyond that.

#### Scenario: exact all-pairs bypassed above the cap (AC-3)

- **WHEN** edge betweenness is requested for a graph with `N > cfgExactBetweennessNodeCap`
- **THEN** the sampled estimator runs over at most `cfgMaxSampledSources` sources with the `N/|S|` rescale applied; the exact O(N·M) pass is not executed (unit-testable with a small synthetic cap)

#### Scenario: sampled estimator is unbiased (AC-4)

- **WHEN** the sampled estimator is run with `s = N` (all sources, cap ≥ N)
- **THEN** `ŷ_V` equals the exact all-pairs Brandes result exactly (relative difference 0), because the `N/s` rescale with `s = N` is the identity

#### Scenario: sampled relative error within ε on sparse graphs (AC-2)

- **WHEN** exact `BC` and sampled `ŷ_S` are computed on a sparse synthetic graph (`N ∈ {500, 2000}`, `M ≈ 5N`, deterministic construction) with a deterministic sample of `s` sources
- **THEN** `‖ŷ_S − BC‖₂ / ‖BC‖₂ ≤ ε` for the configurable `ε` (default 0.1), verified in the AVI-573 analysis spec
