## ADDED Requirements

### Requirement: Haskell library template

The homelab repository SHALL contain a reusable Haskell library template at
`templates/haskell-lib-template/` with: Cabal 3.0 library scaffold (with
`NAME`/`VERSION` placeholders), `devenv.nix` providing GHC 9.10 + cabal + HLS,
MIT `LICENSE`, `.github/workflows/ci.yml` running `cabal build` and
`cabal test` on a self-hosted runner, `.github/workflows/hackage.yml` for
tag-triggered Hackage upload, `cabal.project`, `.gitignore`, `README.md`,
`CHANGELOG.md`, a minimal exposed module, and an hspec-discover test entry
point.

#### Scenario: template is self-contained
- **WHEN** `templates/haskell-lib-template/` is copied to a new directory
- **THEN** `devenv shell` activates GHC 9.10 and cabal
- **AND** `cabal build` succeeds with no source changes
- **AND** `cabal test` passes the stub hspec suite

#### Scenario: CI workflow uses self-hosted runner
- **WHEN** a push triggers the CI workflow
- **THEN** the job runs on a runner with labels `self-hosted` and `kubernetes`
- **AND** the job executes `cabal build` followed by `cabal test`

### Requirement: Library scaffolding script

The homelab repository SHALL provide `scripts/create-haskell-lib.sh` that
takes a library name and description, copies the template, replaces
placeholders, initializes a git repository, creates a public GitHub repo via
`gh repo create --public`, and pushes the initial commit.

#### Scenario: script creates a public repo
- **WHEN** `scripts/create-haskell-lib.sh graphos-types "Shared Graphos types"` is run
- **THEN** a new directory `graphos-types/` is created from the template
- **AND** a public GitHub repository `jeremiepas/graphos-types` is created
- **AND** the initial commit is pushed to `main`

### Requirement: graphos-types package

The system SHALL extract `Domain/Types.hs` and all `Domain/Types/*`
sub-modules into a separate Hackage package `graphos-types` containing the
shared `Node`, `Edge`, `Graph`, `Relation`, `Confidence`, `CommunityId`,
`NodeId`, `EdgeId`, `FileType`, and `CommunityMap` types with their
`FromJSON`/`ToJSON` instances. The package SHALL have zero dependencies on
any other `graphos-*` package.

#### Scenario: graphos-types builds standalone
- **WHEN** `cabal build` is run in the `graphos-types` repo
- **THEN** it compiles without referencing the `graphos` package
- **AND** all type modules and their Aeson instances are exposed

#### Scenario: graphos-types is published
- **WHEN** a tag is pushed to the `graphos-types` repo
- **THEN** the Hackage workflow uploads `graphos-types-0.1.0.0` to Hackage
- **AND** `cabal install graphos-types` succeeds in a clean shell

### Requirement: graphos-leiden package

The system SHALL extract `Domain/Community.hs` and `Domain/Community/Label.hs`
into a separate Hackage package `graphos-leiden` implementing the Leiden
community-detection algorithm. The package SHALL depend on `graphos-types` and
expose `detectCommunities`, `detectCommunitiesWithResolution`,
`cohesionScore`, `scoreAllCohesion`, `Resolution`, `MergeStrategy`,
`mergeSmallCommunities`, `selectRepresentatives`, `filterEdgesByNodeSet`,
`CommunityStats`, and `computeCommunityStats`.

#### Scenario: graphos-leiden builds standalone
- **WHEN** `cabal build` is run in the `graphos-leiden` repo
- **THEN** it compiles depending only on `graphos-types` and external libs
- **AND** `cabal test` passes the community-detection test suite

#### Scenario: graphos-leiden is published
- **WHEN** a tag is pushed to the `graphos-leiden` repo
- **THEN** the Hackage workflow uploads `graphos-leiden-0.1.0.0`
- **AND** `cabal install graphos-leiden` succeeds in a clean shell without
  installing the `graphos` executable package

### Requirement: graphos-cypher package

The system SHALL extract `Domain/Query/Cypher/AST.hs`,
`Domain/Query/Cypher/Parser.hs`, `Domain/Query/Cypher/Eval.hs`, and
`Domain/Query/Cypher/Mapping.hs` into a separate Hackage package
`graphos-cypher` providing an OpenCypher subset AST, parser, and evaluator.
The package SHALL depend on `graphos-types` and expose `CypherQuery`,
`PatternElem`, `NodePat`, `RelPat`, `RelDir`, `HopRange`, `Predicate`,
`CompareOp`, `PropRef`, `ReturnClause`, `ReturnItem`, `OrderItem`, and `Expr`.

#### Scenario: graphos-cypher builds standalone
- **WHEN** `cabal build` is run in the `graphos-cypher` repo
- **THEN** it compiles depending only on `graphos-types` and external libs
- **AND** `cabal test` passes the Cypher parser and evaluator tests

#### Scenario: graphos-cypher is published
- **WHEN** a tag is pushed to the `graphos-cypher` repo
- **THEN** the Hackage workflow uploads `graphos-cypher-0.1.0.0`
- **AND** `cabal install graphos-cypher` succeeds in a clean shell

### Requirement: graphos-context package

The system SHALL extract `Domain/Context.hs`, `UseCase/SelectContext.hs`,
and `UseCase/FormatContext.hs` into a separate Hackage package
`graphos-context` providing LLM context-graph selection and token-budgeted
formatting. The package SHALL depend on `graphos-types` and expose
`QueryComplexity`, `ContextBudget`, `SelectedContext`, `SelectionStrategy`,
`ConversationNode`, `ConversationRelation`, `defaultBudget`,
`budgetForComplexity`, and `emptySelectedContext`.

#### Scenario: graphos-context builds standalone
- **WHEN** `cabal build` is run in the `graphos-context` repo
- **THEN** it compiles depending only on `graphos-types` and external libs
- **AND** `cabal test` passes the context-selection and formatting tests

#### Scenario: graphos-context is published
- **WHEN** a tag is pushed to the `graphos-context` repo
- **THEN** the Hackage workflow uploads `graphos-context-0.1.0.0`
- **AND** `cabal install graphos-context` succeeds in a clean shell

### Requirement: graphos-lsp-extract package

The system SHALL extract `Infrastructure/LSP/Protocol.hs`,
`Infrastructure/LSP/Transport.hs`, `Infrastructure/LSP/Client.hs`,
`Infrastructure/LSP/Capabilities.hs`, `Infrastructure/LSP/CapabilityParse.hs`,
`Infrastructure/LSP/Extraction.hs`, and `Infrastructure/LSP/ServerMap.hs` into
a separate Hackage package `graphos-lsp-extract` providing an LSP client
specialized for code-structure extraction. The package SHALL have zero
dependencies on any `graphos-*` package and expose `LSPClient`,
`LSPClientConfig`, `defaultLSPConfig`, `connectToLSP`, `disconnectLSP`,
`extractDocumentSymbols`, `extractCallHierarchy`, `extractReferences`,
`extractWorkspaceSymbols`, `parseServerCapabilities`, `findLSPServer`, and
`languageIdFromExt`.

#### Scenario: graphos-lsp-extract builds standalone
- **WHEN** `cabal build` is run in the `graphos-lsp-extract` repo
- **THEN** it compiles depending only on external libs (no `graphos-*` deps)
- **AND** `cabal test` passes the LSP client and transport tests

#### Scenario: graphos-lsp-extract is published
- **WHEN** a tag is pushed to the `graphos-lsp-extract` repo
- **THEN** the Hackage workflow uploads `graphos-lsp-extract-0.1.0.0`
- **AND** `cabal install graphos-lsp-extract` succeeds in a clean shell

### Requirement: graphos-graph-export package

The system SHALL extract `Infrastructure/Export/Neo4j.hs`,
`Infrastructure/Export/GraphML.hs`, `Infrastructure/Export/Obsidian.hs`, and
`Infrastructure/Export/JSON.hs` into a separate Hackage package
`graphos-graph-export` unifying Neo4j Cypher statement generation, GraphML,
Obsidian vault, and JGF output. The package SHALL depend on `graphos-types`
and expose `exportCypher`, `pushToNeo4j`, `exportGraphML`, `exportObsidian`,
and the JGF serializer.

#### Scenario: graphos-graph-export builds standalone
- **WHEN** `cabal build` is run in the `graphos-graph-export` repo
- **THEN** it compiles depending only on `graphos-types` and external libs
- **AND** `cabal test` passes the export test suite

#### Scenario: graphos-graph-export is published
- **WHEN** a tag is pushed to the `graphos-graph-export` repo
- **THEN** the Hackage workflow uploads `graphos-graph-export-0.1.0.0`
- **AND** `cabal install graphos-graph-export` succeeds in a clean shell

### Requirement: Self-hosted CI runners

The homelab repository SHALL deploy one self-hosted GitHub Actions runner per
library repository (6 total) on the K3s cluster using the existing
`scripts/create-runner.sh` pattern, each labeled `self-hosted,kubernetes,<lib>`.

#### Scenario: runner for each library repo
- **WHEN** the runner deployments are applied
- **THEN** each library repo's CI jobs run on its dedicated runner
- **AND** the runner pod is labeled with the library name

### Requirement: Graphos depends on published packages

The `graphos.cabal` SHALL add `graphos-types`, `graphos-leiden`,
`graphos-cypher`, `graphos-context`, `graphos-lsp-extract`, and
`graphos-graph-export` to its `build-depends` and SHALL remove the
corresponding modules from `exposed-modules` and from `src/`. The Graphos CLI
behaviour and output formats SHALL remain unchanged.

#### Scenario: Graphos builds with Hackage deps
- **WHEN** `cabal build --flag dev` is run in the Graphos repo
- **THEN** it compiles importing the published packages
- **AND** no extracted source files remain under `src/Graphos/`

#### Scenario: Graphos tests pass
- **WHEN** `cabal test` is run in the Graphos repo
- **THEN** all existing Hspec specs pass with no regressions