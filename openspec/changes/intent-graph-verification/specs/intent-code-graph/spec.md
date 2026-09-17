# intent-code-graph

The typed, deterministic, well-formed graph projection consumed by the certified
checker. No model in the loop: node identity, kind classification and edge endpoints are
all deterministic, because checker verdicts must be stable across runs and the Lean
completeness theorem (O2) assumes well-formedness as a hypothesis.

## ADDED Requirements

### Requirement: Deterministic intent-kind classification

The projection SHALL assign each node exactly one intent kind among `handler`,
`validator`, `test`, `schema-field`, `contract-field`, `doc`, `module`, `other`, using
only deterministic, configuration-declared rules (path globs, LSP symbol kind, name
patterns, annotations). A node matching no rule SHALL be classified `other`. The
classifier SHALL NOT invoke any model, and two runs over the same input SHALL produce
identical classifications.

- **Plan**: Reproducible verdicts require deterministic identity and classification —
  the same reason `spec-graph-verification` parses spec structure without a model.
- **Do**: Pure Domain classifier over config rules; config schema in Infrastructure.
- **Check**: Scenarios below.
- **Act**: If projects need richer taxonomies, extend the kind enum and config schema —
  never the determinism rule.

#### Scenario: Classification is rule-driven and total

- **WHEN** a node's path matches the configured test glob and another node matches no
  rule
- **THEN** the first is classified `test`, the second `other`, and re-running the
  projection yields the identical classification

#### Scenario: No model call in the projection

- **WHEN** the projection runs with every LLM provider disabled
- **THEN** it completes with full classifications

### Requirement: Well-formed, identity-stable checker input

The exported projection SHALL contain only edges whose source and target are declared
nodes, SHALL preserve `NodeId` verbatim from the source graph so that before/after
correspondence is the identity on ids, and SHALL carry each code node's LSP signature
and `body_hash` where the extractor provided them. The loader SHALL reject an ill-formed
projection with one error naming the dangling endpoint.

- **Plan**: Well-formedness is the hypothesis of the Lean completeness theorem
  `reach_complete`; identity-stable ids are the change correspondence µ; signature plus
  body hash enable the `stableSignature` discharge rule.
- **Do**: UseCase projection; Infrastructure export/load with the well-formedness check;
  QuickCheck property for endpoint declaration.
- **Check**: Scenarios below.
- **Act**: If external producers want to feed the checker, publish the projection shape
  alongside the graph.json contract docs.

#### Scenario: Dangling endpoint is rejected

- **WHEN** a projection containing an edge to an undeclared node id is loaded
- **THEN** the load fails with a single error naming that node id

#### Scenario: Ids are stable across before and after

- **WHEN** the same symbol exists in the before and after exports of a change
- **THEN** it carries the same `NodeId` in both

#### Scenario: Body-only edit is visible to rules

- **WHEN** a function's body changes but its signature does not
- **THEN** the after projection shows the same signature and a different `body_hash` for
  that node
