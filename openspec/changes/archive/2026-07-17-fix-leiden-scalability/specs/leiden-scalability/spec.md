# Leiden Scalability Capability

## ADDED Requirements

### Requirement: Constant-time node moves in local moving

The Leiden local moving pass (PRD §5) MUST update community assignments in O(1) per node move, with at most O(n) amortized vector copying per pass (thaw/freeze). It SHALL NOT copy the full assignment vector per individual move.

#### Scenario: Clustering results are unchanged by the optimization

- **WHEN** the deterministic reference graphs from the community test suite are clustered before and after the change
- **THEN** the resulting community maps are identical, verifiable by `cabal test`

#### Scenario: Repository-scale clustering does not regress

- **WHEN** the pipeline runs on the Graphos repository with debug tracing enabled
- **THEN** `span_cluster` duration is at most the pre-change baseline for the same input

#### Scenario: Large graphs cluster within target order of magnitude

- **WHEN** a sparse synthetic graph of at least 50,000 nodes is clustered
- **THEN** clustering completes in seconds (not minutes), consistent with the PRD §16.1 target of under 30 seconds at 100k nodes

### Requirement: Batched refinement updates

The refinement phase MUST apply node reassignments without per-node full-vector copies; updates SHALL be applied in bulk (single copy per pass) or via in-place mutation.

#### Scenario: Refinement preserves assignments semantics

- **WHEN** a graph whose refinement splits a weakly-connected community is clustered
- **THEN** the split result equals the pre-change implementation's output, verifiable by `cabal test`

### Requirement: Incremental merge-phase index

Small-community merging MUST build the node-to-community reverse index once and update it incrementally per merge, rather than rebuilding it for every merged community. Lookups against the index MUST reflect all previously executed merges.

#### Scenario: Merge targets match a recomputed index

- **WHEN** a graph containing several communities below the minimum size is merged
- **THEN** each small community merges into the same target as would be chosen with a freshly recomputed index at each step

### Requirement: Fully forcing LeidenState NFData

The `NFData` instance for the Leiden working state MUST force all fields to normal form so that `deepseq` between iterations prevents thunk accumulation.

#### Scenario: Deep evaluation completes over the whole state

- **WHEN** `rnf` is applied to a constructed Leiden state with non-trivial neighbor and assignment data
- **THEN** evaluation touches all fields and completes without leaving unevaluated structure, verifiable by `cabal test`
