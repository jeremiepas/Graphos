# Import Resolution Capability

## ADDED Requirements

### Requirement: Canonical module node identity

Haskell stub extraction (PRD §3.2 extract stage) MUST assign module nodes and import-target nodes a canonical node ID derived from the module name, such that an `import X` edge in any file resolves to the same node as the module node produced for the file that declares `module X`. The `Main` module SHALL be exempt and keep a directory-scoped ID to avoid collisions between distinct executables.

#### Scenario: Import edge reaches the imported module node

- **WHEN** file A declares `module Graphos.Domain.Config` and file B in a different directory contains `import Graphos.Domain.Config`
- **THEN** the built graph contains exactly one node for `Graphos.Domain.Config`, and B's module node has an `imports` edge whose target is that node

#### Scenario: Main modules do not merge

- **WHEN** two files in different directories both declare `module Main`
- **THEN** the built graph contains two distinct `Main` nodes

### Requirement: Cross-file connectivity

The built graph for a multi-module Haskell codebase MUST contain `imports` edges whose source and target nodes originate from different source files (PRD §3.3 build stage). The graph SHALL NOT consist solely of per-file islands.

#### Scenario: Repository-scale connectivity

- **WHEN** the full pipeline runs on the Graphos repository itself
- **THEN** `graph.json` contains at least one `imports` edge whose source and target have different `source_file` values, and the number of connected components is strictly less than the number of source files

### Requirement: Relation semantics for stub edges

Stub extraction MUST emit `imports` edges only for targets parsed from import declarations, and `contains` edges for module-to-declaration relationships. Declarations SHALL NOT appear as targets of `imports` edges.

#### Scenario: Declarations are contained, not imported

- **WHEN** a Haskell file with one import and one top-level function is stub-extracted
- **THEN** the extraction has exactly one `imports` edge (module → imported module) and one `contains` edge (module → function), verifiable by `cabal test`
