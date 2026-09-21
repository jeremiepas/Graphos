## ADDED Requirements

### Requirement: Co-location doc-code edges

The system SHALL create a `documents` edge from a documentation file to code
files located in the same directory or a descendant directory.

#### Scenario: README links to sibling code
- **WHEN** `libraries/jwt-verifier/README.md` and `libraries/jwt-verifier/src/lib.rs` exist
- **THEN** a `documents` edge connects the README to nodes of `src/lib.rs`

#### Scenario: unrelated doc not linked by co-location
- **WHEN** a README exists in a different top-level directory from a code file
- **THEN** no co-location `documents` edge is created between them

### Requirement: Symbol-mention doc-code edges

The system SHALL create a `documents` edge from a documentation node to the code
node that defines an identifier when that identifier appears in the doc text.

#### Scenario: doc mentions a defined symbol
- **WHEN** a doc contains the token `set_remote_execution` and a code node defines `set_remote_execution`
- **THEN** a `documents` edge connects the doc node to that defining node

#### Scenario: common word not treated as symbol
- **WHEN** a doc contains a common word that is not a defined identifier
- **THEN** no symbol-mention edge is created

### Requirement: Path-reference doc-code edges

The system SHALL create a `documents` edge from a documentation node to the code
nodes of a file when the doc text cites that file's repository-relative path,
including when the doc and the cited file are in different directory subtrees.

#### Scenario: doc cites a file path in another subtree
- **WHEN** `docs/adr/0007-task-model.md` contains the path `src/domain/workflow/task-definition.ts` and code nodes exist for that file
- **THEN** a `documents` edge connects the doc node to nodes of `src/domain/workflow/task-definition.ts`

#### Scenario: unresolved path not linked
- **WHEN** a doc contains a path-like token that does not match any code node's source file
- **THEN** no path-reference edge is created

#### Scenario: bare filename without separator skipped
- **WHEN** a doc mentions a bare filename with no directory separator (e.g. `index.ts`)
- **THEN** no path-reference edge is created, because it is ambiguous across the tree

### Requirement: Edge confidence tagging

The system SHALL tag co-location, symbol-mention, and path-reference edges with
the `documents` relation and high confidence, distinct from similarity-based
`inferred` edges.

#### Scenario: documents edges survive semantic filter
- **WHEN** a query is run with `edges = semantic`
- **THEN** `documents` edges are retained
- **AND** low-confidence `inferred` doc↔code edges may be dropped

### Requirement: Formal verification of the linking guarantees

The change SHALL ship a Lean 4 model of the three passes that compiles clean
(zero errors, zero warnings, zero `sorry`) under the pinned core toolchain, and
whose guarantee theorems are kernel-checked: co-location edges stay within the
doc's directory subtree, symbol-mention edges target the unique definition of a
whole-word identifier, and path-reference edges resolve to an existing source
file.

#### Scenario: Clean rebuild of the Lean model is green
- **WHEN** `lake clean && lake build` is run in the change's `lean/` directory with the pinned Lean 4 core toolchain
- **THEN** the command exits 0 with zero errors, zero warnings, and zero `sorry`, per `lean-proof-methodology`

#### Scenario: Domain passes match the Lean model
- **WHEN** the Domain linking passes run on the Lean model's toy fixtures
- **THEN** the Haskell verdicts are identical to the Lean model's verdicts
