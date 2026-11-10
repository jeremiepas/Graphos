## ADDED Requirements

### Requirement: Configuration sweep

`graphos clustereval sweep` SHALL run Leiden with the auto-tuned resolution and
every agglomerative configuration of the grid, score each against the ground
truth with MoJoFM, and rank them by MoJoFM descending, then by fewer singleton
clusters, then by configuration name. Each row SHALL report the configuration,
MoJoFM, cluster count, singleton count and modularity.

#### Scenario: ranking order
- **WHEN** two configurations have MoJoFM 80 and 90
- **THEN** the 90 configuration is ranked first

#### Scenario: tie broken by singletons
- **WHEN** two configurations have the same MoJoFM and 3 and 7 singleton clusters
- **THEN** the 3-singleton configuration is ranked first

#### Scenario: Leiden always present
- **WHEN** the sweep runs on a graph above the agglomerative file cap
- **THEN** the ranking contains exactly the Leiden row and the report lists the skipped grid

### Requirement: Auto algorithm selection

With `--cluster-algorithm auto`, the build SHALL run the sweep after the build
stage and assign every node the cluster of its file in the winning partition,
and SHALL write the full ranking to `cluster_eval.json` in the output directory.

#### Scenario: winner applied
- **WHEN** a build runs with `--cluster-algorithm auto` and an agglomerative configuration wins
- **THEN** every symbol node's community equals its file's cluster in that configuration and `cluster_eval.json` names it as `selected`
