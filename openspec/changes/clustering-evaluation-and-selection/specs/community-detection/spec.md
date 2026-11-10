## ADDED Requirements

### Requirement: Selectable clustering algorithm

The cluster stage SHALL accept `--cluster-algorithm leiden|agglomerative|auto`
and config key `cluster.algorithm`, defaulting to `leiden`. With `leiden` the
partition SHALL be identical to the partition produced before this option
existed. With `agglomerative` the stage SHALL use `--linkage`, `--distance` and
`--cluster-divisor` (defaults: single, cosine, 25). (PRD §5.1)

#### Scenario: default unchanged
- **WHEN** a build runs without `--cluster-algorithm`
- **THEN** the communities equal those of the same build with `--cluster-algorithm leiden`

#### Scenario: agglomerative build
- **WHEN** a build runs with `--cluster-algorithm agglomerative --linkage average --distance manhattan --cluster-divisor 10`
- **THEN** `graph.json` holds communities whose file-level partition has ⌈F / 10⌉ clusters
