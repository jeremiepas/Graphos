## ADDED Requirements

### Requirement: Structural search landscape

`graphos clustereval landscape` SHALL run the Bunch search `--runs N` times
(default 100) with seeds `s..s+N−1` on the file MDG of `--graph` and SHALL
record per run: seed, MQ and cluster count of the random starting partition,
final MQ, final cluster count, intra-edge fraction, number of MQ evaluations,
iterations and annealing acceptances. Runs SHALL be reported sorted by final
MQ ascending. The system SHALL group runs into basins by rounding final MQ to
the nearest 0.05 and SHALL list every basin holding at least 5% of runs with
its MQ range and cluster-count range. (PRD §5.1)

#### Scenario: record count and order
- **WHEN** `graphos clustereval landscape --graph g.json --runs 20 --seed 3` runs
- **THEN** 20 records with seeds 3..22 are emitted, sorted by final MQ ascending

#### Scenario: basins
- **WHEN** 60 runs end at MQ 1.80–1.84 and 40 at MQ 2.30–2.31
- **THEN** two basins are listed, with 60% and 40% of runs and their cluster-count ranges

### Requirement: Edge similarity distribution

For every MDG edge (u, v) the system SHALL count the runs in which u and v
share a cluster, divide by N to obtain `P_uv`, and SHALL report the share of
|E| in each bucket: Zero (`P = 0`), Low (`0 < P ≤ 0.10`), Medium
(`0.10 < P < 0.75`), High (`P ≥ 0.75`). The four shares SHALL sum to 100%.

#### Scenario: identical runs
- **WHEN** every run produces the same partition
- **THEN** every edge is in Zero or High and Low = Medium = 0%

#### Scenario: drifting edge
- **WHEN** an edge is intra-cluster in half of the runs
- **THEN** it is counted in Medium

### Requirement: Random-partition baseline and Leiden point

The system SHALL score `--random-partitions M` (default 1,000) random
partitions of the MDG with MQ and SHALL report their minimum, median and
maximum. When the graph carries communities, the system SHALL fold them to
files by majority community and SHALL report that partition's MQ, cluster
count and intra-edge fraction as the `leiden` point.

#### Scenario: baseline below search
- **WHEN** the landscape runs on the two-clique fixture
- **THEN** the maximum baseline MQ is below the minimum final MQ of the runs

#### Scenario: no communities
- **WHEN** the graph was built with `--no-cluster`
- **THEN** the `leiden` point is absent from the output

### Requirement: Random control graphs

With `--random-graph rnd:N:D` or `bip:N:D` the system SHALL generate, from
`--seed`, an MDG of N nodes and `⌊D · N(N−1)/2⌋` distinct undirected
edges chosen uniformly without replacement — for `bip` only between two halves
of ⌊N/2⌋ and ⌈N/2⌉ nodes, saturating at the ⌊N/2⌋·⌈N/2⌉ cross pairs — and
SHALL run the landscape on it instead of a graph file.

#### Scenario: edge counts of the paper
- **WHEN** `rnd:100:0.05`, `rnd:100:0.5` and `bip:100:0.25` are generated
- **THEN** they have 247, 2,475 and 1,237 edges respectively and `bip` has no edge inside a half

#### Scenario: bipartite saturation
- **WHEN** `bip:100:0.75` is generated (nominally 3,712 edges)
- **THEN** it is the complete bipartite graph of 2,500 cross edges, since two halves of 50 have no more

#### Scenario: seeded
- **WHEN** `rnd:100:0.05` is generated twice with `--seed 1`
- **THEN** the two edge sets are equal

### Requirement: Landscape outputs

The command SHALL write `landscape.json` atomically to `--out` (default the
graph's directory) with keys `config`, `runs`, `basins`, `similarity`,
`random_baseline` and, when present, `leiden`, and SHALL print a markdown
summary (basins table, similarity table, baseline, Leiden point). With
`--json` stdout SHALL be the JSON document only.

#### Scenario: JSON keys
- **WHEN** `graphos clustereval landscape --graph g.json --json` runs
- **THEN** stdout is one JSON object containing `config`, `runs`, `basins`, `similarity` and `random_baseline`

#### Scenario: file written
- **WHEN** the command completes without `--json`
- **THEN** `landscape.json` exists in the output directory and the summary names every basin
