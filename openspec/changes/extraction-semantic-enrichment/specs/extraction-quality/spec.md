# Extraction Quality

## ADDED Requirements

### Requirement: Declaration names come from the grammar name field

The identifier of an emitted definition node SHALL be taken from the
declaration's tree-sitter `name` field when the grammar provides one; the
language-neutral identifier-type scan applies only as fallback, and within
that fallback an `identifier` child SHALL be preferred over a
`type_identifier` child. A method or function node's label SHALL never be its
return type.

#### Scenario: Java method label is the method name

- **WHEN** a Java class declares
  `public ClientData retrieveClient(final long id) { … }`
- **THEN** the emitted Method node has label `retrieveClient` and an id ending
  in `Method:retrieveClient`

#### Scenario: methods sharing a return type do not collide

- **WHEN** a Java class declares two methods both returning `ClientData`
- **THEN** the two Method nodes have distinct ids derived from their distinct
  names and each keeps its own `contains` edge from the class

### Requirement: Reference-only AST types emit no node

A bare type reference (`type_identifier` outside a declaring position) SHALL
NOT be emitted as a node. No emitted node may carry an empty base id.

#### Scenario: field type annotations produce no nodes

- **WHEN** a Java file declares `ClientData d = helper(id);` at fine
  granularity
- **THEN** no node of kind `type_identifier` and no node whose id is empty or
  a bare `#L<line>` suffix exists in the extraction

### Requirement: Definition signatures are declaration headers

For a definition node whose grammar exposes a body, `nodeSignature` SHALL
contain the declaration header only — annotations/decorators/attributes,
modifiers, name, parameters and return type — normalized and truncated under
the existing budget, and SHALL NOT include body text. Module and import
signature rules are unchanged.

#### Scenario: annotated method signature keeps annotations, drops the body

- **WHEN** a Java method is declared as
  `@GET @Path("/{id}") public ClientData retrieveClient(final long id) { ClientData d = helper(id); return d; }`
- **THEN** its signature starts with `@GET @Path("/{id}")`, contains
  `retrieveClient(final long id)`, and does not contain `helper(id)`
