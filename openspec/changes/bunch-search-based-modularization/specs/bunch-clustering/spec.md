## ADDED Requirements

### Requirement: File-level module dependency graph

The system SHALL fold a graph of any granularity to a module dependency graph
whose nodes are the distinct `nodeSourceFile` values and whose undirected
edges join two files when any edge of a relation other than `contains` links
a node of one file to a node of the other. Self-pairs SHALL be dropped. The
weight of a file pair SHALL be 1, or the sum of the folded edges' `edgeWeight`
when `--bunch-weighted` is set. Files SHALL be indexed in ascending path order
before any random draw. (PRD §5.1)

#### Scenario: symbol edges fold to one file pair
- **WHEN** functions in `a.ts` call functions in `b.ts` three times and `b.ts` imports `a.ts`
- **THEN** the MDG has one edge `{a.ts, b.ts}` of weight 1, or weight 4 when weighted

#### Scenario: contains-only graph
- **WHEN** the only edges are `contains` edges from files to their symbols
- **THEN** the MDG has the files as nodes and no edges

#### Scenario: node order does not matter
- **WHEN** the same graph is folded from two different node orders
- **THEN** the file indices and adjacency are identical

### Requirement: Hill-climbing search over partitions

With `--cluster-algorithm bunch` the system SHALL start from a random
partition (cluster count drawn uniformly from 1..n, each file assigned
uniformly) and SHALL repeatedly replace it by a candidate with strictly higher
MQ until no candidate improves it or `--bunch-max-iterations` (default 10,000)
is reached. Candidates SHALL be the neighbouring partitions (one file moved to
another cluster or to a new singleton) followed by the building-block
partitions (one new cluster of exactly two files joined by an MDG edge),
enumerated in canonical order: files ascending, target clusters ascending,
new singleton, then building blocks by ascending file pair. The threshold
`--bunch-threshold η` (0–100, default 0) SHALL require at least ⌈η · |C|⌉ of
the |C| candidates to be examined per iteration before the best examined
improvement is taken; η = 0 SHALL take the first improvement and η = 100
SHALL take the best of all candidates. (PRD §5.1)

#### Scenario: two cliques
- **WHEN** the MDG is two disjoint 5-cliques joined by one edge
- **THEN** for every seed in 0..31 the search converges to the two cliques with MQ = 2·(2·10 / (2·10 + 1))

#### Scenario: MQ never decreases without annealing
- **WHEN** the search runs with annealing disabled
- **THEN** the MQ of the current partition is non-decreasing across iterations

#### Scenario: threshold changes work, not result
- **WHEN** the two-clique fixture is searched with η ∈ {0, 50, 100} from the same seed
- **THEN** the three runs reach the same MQ and the number of MQ evaluations is non-decreasing in η

#### Scenario: building block opens a cluster
- **WHEN** the current partition has every file in one cluster and the MDG has two disjoint edges
- **THEN** a building-block candidate exists for each edge and at least one has higher MQ

### Requirement: Simulated annealing option

The system SHALL, when `--bunch-sa T0,alpha` is set (defaults `T0 = 100`,
`alpha = 0.99`), accept a non-improving candidate with probability
`exp(ΔMQ / T)` for `ΔMQ < 0`, SHALL set `T ← alpha · T` after every
probability draw, and SHALL count the accepted non-improving candidates. With
annealing disabled the count SHALL be 0.

#### Scenario: annealing off
- **WHEN** the search runs without `--bunch-sa`
- **THEN** zero non-improving candidates are accepted

#### Scenario: annealing on
- **WHEN** the two-clique fixture is searched with `--bunch-sa 100,0.99`
- **THEN** the search terminates and reports the number of accepted non-improving candidates

### Requirement: Population and determinism

The system SHALL run `--bunch-population p` (default 8) independent searches
from seeds `s, s+1, …, s+p−1` where `s` is `--seed` (default 0), SHALL keep
the partition with the highest MQ (lowest seed on ties), and SHALL relabel its
clusters 0..k−1 by ascending smallest member file. For a fixed graph,
configuration and seed the resulting `CommunityMap` SHALL be identical across
runs and across node orderings of the input graph.

#### Scenario: reproducible
- **WHEN** the same graph is clustered twice with `--cluster-algorithm bunch --seed 7`
- **THEN** the two `CommunityMap`s are equal

#### Scenario: permutation invariant
- **WHEN** the same graph is supplied with its nodes in two different orders
- **THEN** every node receives the same community id in both runs

### Requirement: File cap and symbol push-back

The system SHALL skip Bunch and run Leiden when the MDG has more files than
`--bunch-max-files` (default 5,000), logging the file count and the cap. When
Bunch runs, every symbol node SHALL receive its file's cluster, the
community-size cap post-pass SHALL apply as for Leiden, and
`mergeSmallCommunities` SHALL NOT apply.

#### Scenario: cap exceeded
- **WHEN** a graph folds to 6,000 files and the cap is 5,000
- **THEN** the partition equals the Leiden partition of the same build and the log names the skip with 6,000 and 5,000

#### Scenario: symbols follow their file
- **WHEN** Bunch places `a.ts` in cluster 3
- **THEN** every node whose `nodeSourceFile` is `a.ts` has community 3 in `graph.json`
