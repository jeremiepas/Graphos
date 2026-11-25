## Purpose

The `graphos-studio` repository owns Graphos' frontend — the Elm Studio SPA and the static HTML viewer — building, testing, and serving them independently of the Haskell backend while consuming the `graph-json-contract` shape and the `graphos serve` slice/query API.

## ADDED Requirements

### Requirement: Frontend lives in a dedicated repository

The Graphos frontend SHALL live in a dedicated `graphos-studio` repository, separate from the Haskell `graphos` repository. That repository SHALL own the Elm Studio sources and tests and the HTML viewer assets (`viewer.js`, `viewer.css`, and the vendored renderer). The Haskell repository SHALL NOT contain the studio sources, the viewer sources, or their build tooling.

#### Scenario: Single frontend home

- **WHEN** a developer wants to change the studio or the viewer
- **THEN** the change is made in the `graphos-studio` repository, not the Haskell repository

#### Scenario: Haskell repo is free of frontend sources

- **WHEN** the Haskell repository is inspected after the split
- **THEN** the `studio/` directory, the `assets/viewer/` source files, and the Elm toolchain are absent from it

### Requirement: Frontend builds and tests independently

The `graphos-studio` repository SHALL build and test the studio and the viewer with its own CI and dev shell, with no dependency on the Haskell repository's `cabal` build. A failing frontend build or test SHALL fail the `graphos-studio` repository's CI independently of the Haskell repository.

#### Scenario: Independent build

- **WHEN** the `graphos-studio` repository's CI runs
- **THEN** it builds and tests the studio and viewer without invoking the Haskell build

#### Scenario: Frontend failure does not block the backend

- **WHEN** a frontend change breaks an Elm type or a studio test
- **THEN** the `graphos-studio` CI fails while the Haskell repository's CI is unaffected

### Requirement: graphos serve resolves the frontend bundle

The `graphos serve` command SHALL serve the studio bundle and the viewer assets from the frontend repository's published output (or a configured location) rather than from assets compiled into the Haskell binary. The viewer SHALL NOT be embedded into the Haskell binary at compile time.

#### Scenario: serve renders the frontend

- **WHEN** a user runs `graphos serve` against a graph
- **THEN** the studio and the viewer are reachable and rendered from the frontend bundle, with no viewer code compiled into the Haskell binary

### Requirement: Cross-repository contract is preserved

The frontend SHALL consume the `graph-json-contract` shape and the `graphos serve` slice/query API without change. A contract check SHALL verify that a graph produced by the Haskell repository still decodes through the frontend's graph decoders.

#### Scenario: Haskell output decodes in the frontend

- **WHEN** the Haskell repository produces a `graph.json` fixture and the frontend repository runs its contract test
- **THEN** the fixture decodes through the frontend graph decoders with no contract drift
