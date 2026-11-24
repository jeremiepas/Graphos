## MODIFIED Requirements

### Requirement: Elm application structure

The studio SHALL be an Elm 0.19 single-page application in the `graphos-studio` repository, structured as The Elm Architecture — one `Model`, one `Msg` type, one `update` — with all effects (HTTP, file reads, renderer commands) at the edges via commands and ports, and no JavaScript beyond the port glue and the vendored renderer. `elm make --optimize` SHALL produce a static bundle that `graphos serve` resolves from the frontend repository (or any static host) with no build-time server dependency on the Haskell repository.

#### Scenario: Clean build from the dev shell

- **WHEN** a developer runs `elm make --optimize` in the `graphos-studio` repository's dev shell
- **THEN** it produces the static bundle with no errors and no network access beyond the Elm package cache

#### Scenario: State lives in the Model

- **WHEN** any studio feature (navigation, editing, groups, extraction) changes application state
- **THEN** the change is a `Msg` through `update` — the port glue holds no state beyond renderer handles

### Requirement: Repo and CI integration

The studio SHALL build and test in the `graphos-studio` repository's CI: `elm make --optimize`, `elm-test`, and `elm-review` run there, and a failing studio build SHALL fail that repository's CI. The `graphos-studio` dev shell SHALL provide the complete Elm toolchain so `nix`-based setup is one command. The Haskell repository SHALL NOT provide the Elm toolchain and SHALL NOT run the studio build in its own CI.

#### Scenario: Fresh clone builds the studio

- **WHEN** a developer clones `graphos-studio`, enters its dev shell, and runs the studio build
- **THEN** the build succeeds with no manually installed tools

#### Scenario: CI gates on studio health

- **WHEN** a PR in `graphos-studio` breaks an Elm type or a studio test
- **THEN** the `graphos-studio` CI fails while the Haskell repository's CI reports independently
