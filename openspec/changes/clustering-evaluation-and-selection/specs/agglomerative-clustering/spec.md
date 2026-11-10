## ADDED Requirements

### Requirement: File dependency matrix

The system SHALL fold a graph of any granularity to file nodes and build the
symmetric file dependency matrix `W = A + Aᵀ`, where `A[i][j]` counts edges of
any relation from a node of file i to a node of file j, excluding `contains`
edges and self-pairs.

#### Scenario: symbol edges fold to files
- **WHEN** function `f` in `a.ts` calls `g` and `h` in `b.ts`
- **THEN** `W[a.ts][b.ts] = W[b.ts][a.ts] = 2`

#### Scenario: contains edges ignored
- **WHEN** the only edges are `contains` from files to their symbols
- **THEN** W is the zero matrix

### Requirement: Agglomerative hierarchical clustering

The system SHALL cluster files by agglomerative hierarchical clustering on the
rows of W, with distance ∈ {euclidean, manhattan, cosine} (cosine distance of a
zero row is 1), linkage ∈ {single, average, complete}, and a cut producing
exactly `K = ⌈F / q⌉` clusters for a divisor q (default grid
{5, 7, 10, 15, 20, 25}). Ties SHALL be broken by the smallest cluster-id pair so
that equal inputs give equal partitions.

#### Scenario: two obvious groups
- **WHEN** files {a, b} depend only on each other, {c, d} depend only on each other, and K = 2
- **THEN** every distance/linkage pair returns the partition {{a, b}, {c, d}}

#### Scenario: exact cluster count
- **WHEN** F = 20 and q = 5
- **THEN** the partition has exactly 4 clusters

#### Scenario: determinism
- **WHEN** the same graph is clustered twice with the same configuration
- **THEN** both partitions are identical

### Requirement: File-count cap

The system SHALL skip agglomerative clustering when the file count exceeds
`--agglomerative-max-files` (default 10,000) and SHALL report the skip and its
reason instead of failing.

#### Scenario: cap exceeded
- **WHEN** a graph has 12,000 files and the cap is 10,000
- **THEN** no agglomerative partition is produced and the report lists the skip with the file count
