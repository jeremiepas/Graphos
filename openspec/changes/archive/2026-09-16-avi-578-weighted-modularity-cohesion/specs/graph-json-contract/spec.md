## ADDED Requirements

### Requirement: null_model top-level section

The exporter SHALL write a top-level `null_model` string key recording which
configuration null model the weighted modularity baseline used
(`degree_config_undirected` for the default undirected configuration model,
`degree_config_directed` for the directed variant). The key is additive: legacy
graphs without it load as before.

#### Scenario: Clustered export carries null_model

- **WHEN** a clustered graph is exported with the default analysis
- **THEN** the JSON contains `"null_model": "degree_config_undirected"` alongside `communities`, `cohesion` and `god_nodes`