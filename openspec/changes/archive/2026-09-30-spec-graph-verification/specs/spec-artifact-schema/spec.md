# spec-artifact-schema

The typed vocabulary that turns specs/ADRs/PRDs into graph structure, and the hybrid
extraction that populates it. Trust discipline: structure is parsed deterministically;
the small model contributes semantic edges only, under harness validation.

## ADDED Requirements

### Requirement: Spec-artifact node and edge vocabulary

Graphos SHALL represent specification artifacts as first-class graph structure: node
kinds `Requirement`, `Decision`, `Constraint` and `Scenario` (via the existing free-form
`kind` field), an `active` status for decisions (superseded ADRs remain in the graph,
marked inactive, via `extra`), and edge relations `refines`, `conflicts_with`,
`satisfies`, `supersedes` and `constrains` added to the contract's relation vocabulary.
Every spec-artifact node SHALL carry its `source_file` and line span like any other node,
so findings are clickable.

- **Plan**: The 12-field node schema and relation enum already exist; this is vocabulary,
  not new machinery — `node-schema` and `graph-json-contract` stay authoritative.
- **Do**: Extend the `Relation` enum + its JSON round-trip; document the kinds.
- **Check**: Scenarios below.
- **Act**: If more artifact types appear (RFCs, runbooks), extend kinds — the checks key
  on relations, not on an exhaustive kind list.

#### Scenario: Relations round-trip the contract

- **WHEN** a graph containing a `constrains` edge is exported to graph.json and reloaded
- **THEN** the edge decodes with relation `constrains`, and pre-change consumers reading
  the same file tolerate the unknown relation without failing

#### Scenario: Findings are addressable

- **WHEN** any speccheck finding names a requirement
- **THEN** the finding carries the requirement's `source_file:line` span

### Requirement: Hybrid extraction — deterministic structure, model semantics

Spec-artifact extraction SHALL be layered: (1) a deterministic parser SHALL extract what
the document format already encodes — openspec requirement headers, SHALL/MUST clauses,
`#### Scenario` blocks, ADR status and "Supersedes" lines — producing nodes and
`supersedes`/`contains` edges with no model involvement; (2) the small model SHALL be
asked only for semantic edges (`refines`, `satisfies`, `constrains`, `conflicts_with`
hypotheses, `depends_on`) between already-parsed artifacts, one document (plus its
referenced artifact titles) per call, with structured output validated by the
`extraction-fidelity-harness`; model-emitted edges referencing unknown node ids SHALL be
dropped and counted in the extraction report, never invented into nodes.

- **Plan**: The openspec CLI already parses requirement/scenario structure — the
  deterministic layer is mostly free; keeping the model to per-document semantic edges is
  what makes a 7–27B local model sufficient.
- **Do**: Structural parser in Infrastructure; semantic pass through the existing LLM
  client with a fixed edge-emission schema.
- **Check**: Scenarios below.
- **Act**: If harness validation shows systematic edge hallucination on a model, record
  the model in the harness corpus and require a stronger one in config — never loosen
  the unknown-id drop rule.

#### Scenario: Structure needs no model

- **WHEN** an openspec spec.md with 3 requirements and 7 scenarios is extracted with the
  LLM provider disabled
- **THEN** the graph contains the 3 Requirement and 7 Scenario nodes with correct spans
  and `contains` edges — only semantic edges are missing

#### Scenario: Model edges are validated

- **WHEN** the semantic pass emits an edge whose target id does not exist in the parsed
  graph
- **THEN** the edge is dropped, the drop is counted in the extraction report, and no
  placeholder node is created

#### Scenario: One document per call

- **WHEN** semantic extraction runs over a corpus of 40 spec documents
- **THEN** no model call's input contains more than one document body
