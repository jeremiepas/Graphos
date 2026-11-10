# graph-json-contract (delta)

The contract gains the spec-artifact relation vocabulary. Additive: existing relations,
shapes and consumers are untouched.

## ADDED Requirements

### Requirement: Spec-artifact relation vocabulary

The edge relation vocabulary SHALL include `refines`, `conflicts_with`, `satisfies`,
`supersedes` and `constrains` alongside the existing eight relations, with the same
lowercase text encoding in JSON, and graph.json documents containing them SHALL load in
every graphos consumer (query, cypher, serve, subgraph, diff, studio). Consumers built
before this change SHALL tolerate the new relations per the tolerant-decode default
(`--strict-graph` reserved for explicit strictness).

- **Plan**: One enum extension with round-trip coverage; the tolerant/strict split
  already exists in the loaders.
- **Do**: Extend `Relation` + `relationToText`/`textToRelation`; round-trip Hspec cases.
- **Check**: Scenario below.
- **Act**: If external tooling needs a registry of relations, expose it via
  `renderCommandReference` output rather than a second list.

#### Scenario: New relations load everywhere

- **WHEN** a graph.json containing `satisfies` and `constrains` edges is queried via
  `graphos cypher` and loaded by the studio
- **THEN** both consumers read the edges with their relation intact and no strict-mode
  failure occurs in tolerant mode
