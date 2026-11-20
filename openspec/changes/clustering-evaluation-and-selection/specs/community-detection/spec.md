## ADDED Requirements

### Requirement: Agglomerative and auto clustering algorithms

`--cluster-algorithm` and `clustering.algorithm` SHALL also accept
`agglomerative` and `auto`; the default stays `leiden`. `agglomerative` SHALL
use `--linkage`, `--distance` and `--cluster-divisor` (defaults: single,
cosine, 25) and push each file's cluster to its symbols. `auto` SHALL apply the
configuration sweep's winner (`clustering-selection`). Each returns a
`ClusterQuality` naming its algorithm. (PRD §5.1)

#### Scenario: default unchanged
- **WHEN** a build runs without `--cluster-algorithm`
- **THEN** the communities equal those of the same build with `--cluster-algorithm leiden`, byte for byte

#### Scenario: agglomerative build
- **WHEN** a build runs with `--cluster-algorithm agglomerative --linkage average --distance manhattan --cluster-divisor 10`
- **THEN** `graph.json` holds communities whose file-level partition has ⌈F / 10⌉ clusters, and `ClusterQuality` names `agglomerative`

#### Scenario: existing values kept
- **WHEN** a build runs with `--cluster-algorithm bunch`
- **THEN** it behaves as specified by "Bunch as a selectable clustering algorithm"
