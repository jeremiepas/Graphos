# lean-critique-bridge

## Purpose

Define the contract for the Lean 4 critique library and its compile-time
consumption by Graphos.

## ADDED Requirements

### Requirement: Versioned C ABI
The OSS library SHALL export a stable C ABI with a version query
`graphos_lean_critique_version` and a public header `graphos_lean_critique.h`.

#### Scenario: Consumer checks the linked ABI
- **WHEN** a consumer includes `graphos_lean_critique.h` and calls the version
  query on the linked library
- **THEN** it receives the ABI version the library implements
- **AND** it can compare that value with `GRAPHOS_LEAN_CRITIQUE_ABI_VERSION`
  before calling any other entry point

#### Scenario: Consumer detects a stale build
- **WHEN** the linked library was built from a release older than the header
  the consumer compiled against
- **THEN** the release-version query reports a packed SemVer that differs from
  `GRAPHOS_LEAN_CRITIQUE_SEMVER`
- **AND** the mismatch is reported as a warning, not as an ABI incompatibility

### Requirement: Compile-time linkage
Graphos SHALL link the OSS library at build time via Cabal `extra-libraries`,
`extra-lib-dirs`, and `include-dirs`; it SHALL NOT dynamically load a runtime
bridge.

#### Scenario: Default Graphos build stays toolchain-free
- **WHEN** Graphos is built without the Lean backend enabled
- **THEN** no Lean toolchain and no `libgraphoslean` are required
- **AND** the Haskell critique implementation remains the only backend

#### Scenario: Lean-enabled Graphos build links the release archive
- **WHEN** Graphos is built with the Lean backend enabled and the published
  header and library paths
- **THEN** GHC resolves the foreign calls against the pinned release archive at
  link time
- **AND** no runtime plugin loading or interpreter is involved

### Requirement: Parity
Lean-side critique results SHALL match Haskell results on pinned golden fixtures
within the agreed tolerance.

#### Scenario: Pinned fixtures agree
- **WHEN** the pinned fixture set `(0,1,1,0)=6`, `(0,1,1,1)=10`, `(9,0,0,0)=1`
  is evaluated by the Lean predicate and by the Haskell reference
- **THEN** both return the same scores
- **AND** the Lean-side fixtures fail the library build when they drift

#### Scenario: Candidate ordering is preserved
- **WHEN** the critique surface is served by the Lean predicate
- **THEN** the Haskell ordering of candidates (score descending, then original
  index) is unchanged

### Requirement: Fallback
Graphos SHALL retain the Haskell critique implementation as a reference/optional
fallback during rollout, selectable by build flag or capability check.

#### Scenario: Lean backend unavailable
- **WHEN** the Lean backend is not linked or its reported ABI version does not
  match
- **THEN** Graphos serves the critique surface from the Haskell implementation
- **AND** the fallback selection is observable in configuration or logs
