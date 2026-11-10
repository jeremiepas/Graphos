## ADDED Requirements

### Requirement: Bunch as a selectable clustering algorithm

The cluster stage SHALL accept `--cluster-algorithm leiden|bunch` and config
key `clustering.algorithm`, defaulting to `leiden`. With `leiden` the
partition SHALL be identical to the partition produced before this option
existed. With `bunch` the stage SHALL run the Bunch search of the
`bunch-clustering` capability with `--seed`, `--bunch-population`,
`--bunch-threshold`, `--bunch-sa`, `--bunch-max-iterations`,
`--bunch-max-files` and `--bunch-weighted`, CLI values overriding
`clustering.bunch.*`. The stage SHALL return, next to the partition, a
`ClusterQuality` holding the algorithm name, MQ, modularity and intra-edge
fraction of the final partition. (PRD §5.1)

#### Scenario: default unchanged
- **WHEN** a build runs without `--cluster-algorithm`
- **THEN** the communities equal those of the same build with `--cluster-algorithm leiden`, byte for byte

#### Scenario: bunch build
- **WHEN** a build runs with `--cluster-algorithm bunch --seed 1`
- **THEN** `graph.json` holds communities equal to the Bunch file partition pushed to symbols, and `ClusterQuality` names `bunch`

#### Scenario: config and CLI precedence
- **WHEN** `graphos.yaml` sets `clustering.bunch.threshold: 50` and the build passes `--bunch-threshold 0`
- **THEN** the search runs with η = 0

#### Scenario: invalid threshold
- **WHEN** `--bunch-threshold 101` is passed
- **THEN** the CLI rejects the value and exits non-zero before clustering
