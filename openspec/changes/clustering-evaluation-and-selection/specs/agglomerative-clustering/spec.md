## ADDED Requirements

### Requirement: File dependency matrix

The system SHALL build the symmetric file dependency matrix `W` from the Bunch
file-level module dependency graph (`bunch-clustering`), folded with edge
weights: the files are the MDG files (code files with a declared node — no
package, out-of-corpus placeholder, documentation, image or office file), and
`W[i][j] = W[j][i]` is the summed weight of the edges whose relation is in
`clustering.bunch.relations` and that join a node of file i to a node of file
j. `contains`, `documents`, `depends_on`, `inferred` and self-pairs never
contribute. Agglomerative clustering and Bunch therefore see the same files
and the same dependencies.

#### Scenario: symbol edges fold to files
- **WHEN** function `f` in `a.ts` calls `g` and `h` in `b.ts`
- **THEN** `W[a.ts][b.ts] = W[b.ts][a.ts] = 2`

#### Scenario: contains edges ignored
- **WHEN** the only edges are `contains` from files to their symbols
- **THEN** W is the zero matrix

#### Scenario: packages are not files
- **WHEN** `a.ts` and `b.ts` both import the package `zod`
- **THEN** W has no row for `external:zod` and the import adds nothing between `a.ts` and `b.ts`

### Requirement: Agglomerative hierarchical clustering

The system SHALL cluster files by agglomerative hierarchical clustering on the
rows of W, each with its self entry `W[i][i]` set to the row's largest weight
(so two files that depend only on each other have equal rows; a file with no
dependency keeps a zero row), with distance ∈ {euclidean, manhattan, cosine} (cosine distance of a
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

The system SHALL skip agglomerative clustering when the MDG has more files than
`--agglomerative-max-files` (default 10,000), run Leiden instead and report the
skip with the file count and the cap, as for `--bunch-max-files`
(`bunch-clustering`), instead of failing.

#### Scenario: cap exceeded
- **WHEN** a graph folds to 12,000 MDG files and the cap is 10,000
- **THEN** no agglomerative partition is produced, the partition is Leiden's, and the report lists the skip with 12,000 and 10,000
