# studio-app-shell Specification

## Purpose
TBD - created by archiving change elm-graph-studio. Update Purpose after archive.
## Requirements
### Requirement: Elm application structure

The studio SHALL be an Elm 0.19 single-page application under `studio/`, structured as The
Elm Architecture — one `Model`, one `Msg` type, one `update` — with all effects (HTTP, file
reads, renderer commands) at the edges via commands and ports, and no JavaScript beyond the
port glue and the vendored renderer. `elm make --optimize` SHALL produce a static bundle
servable by `graphos serve` or any static host with no build-time server dependency.

- **Plan**: TEA is the same single-dispatcher model the viewer converged on; keeping JS to
  port glue preserves the property that application state lives in one typed place.
- **Do**: `studio/src/` Elm sources, `studio/elm.json`, one `ports.js`; devenv gains elm,
  elm-test, elm-review.
- **Check**: Scenarios below.
- **Act**: If bundle size becomes a concern, split the renderer port module — never the
  Model.

#### Scenario: Clean build from the dev shell

- **WHEN** a developer runs `elm make --optimize` in `studio/` inside the devenv shell
- **THEN** it produces the static bundle with no errors and no network access beyond the Elm
  package cache

#### Scenario: State lives in the Model

- **WHEN** any studio feature (navigation, editing, groups, extraction) changes application
  state
- **THEN** the change is a `Msg` through `update` — the port glue holds no state beyond
  renderer handles

### Requirement: Navigation with viewer-navigation semantics

The studio SHALL implement the navigation model specified by `viewer-navigation`: the
three-level position (overview ▸ community ▸ node), URL-encoded view state with deep-link
restore and stale-reference degradation to overview, history where position changes push and
tuning changes replace, a clickable breadcrumb derived from the position, and the hierarchical
Escape. The studio's `update` SHALL preserve the invariants proved in that change's Lean model
(escape strictly descends; breadcrumb of the escaped position equals the trail minus its last
segment; non-position actions never alter the back target).

- **Plan**: The semantics are already specified and formally verified once in this repo;
  re-deriving them ad hoc in a second frontend is how two viewers end up disagreeing.
- **Do**: Elm routing over the same position/tuning encoding; the Lean model in
  `openspec/changes/viewer-navigation/lean/` is the reference for `update`'s navigation
  clauses.
- **Check**: Scenarios below.
- **Act**: If studio-specific navigation needs arise (e.g. editor panes), extend the position
  type and update the Lean model in the same commit.

#### Scenario: Deep link restores a studio view

- **WHEN** a studio URL addressing community 483 with node `mod_Auth` selected is opened in a
  fresh tab
- **THEN** the studio restores that position (fetching what it needs per its data source) and
  the breadcrumb shows all three segments

#### Scenario: History semantics match the viewer

- **WHEN** a user drills into a community, selects a node, toggles two group visibilities,
  then presses Back
- **THEN** the studio returns to the community with no node selected — the group toggles did
  not create history entries

#### Scenario: Escape descends

- **WHEN** a user with a node selected presses Escape twice
- **THEN** the first press clears the selection and the second returns to the overview

### Requirement: Repo and CI integration

The studio SHALL build and test in CI: `elm make --optimize`, `elm-test`, and `elm-review`
run as a job alongside the Haskell build, and a failing studio build SHALL fail CI. The
devenv shell SHALL provide the complete Elm toolchain so `nix`-based setup is one command,
consistent with the rest of the repo.

- **Plan**: An app that only builds on one laptop is not a deliverable in this repo's
  conventions.
- **Do**: devenv.nix toolchain addition; CI job reusing the repo's workflow structure.
- **Check**: Scenarios below.
- **Act**: If elm-review noise slows iteration, tune its config in-repo rather than dropping
  the gate.

#### Scenario: Fresh clone builds the studio

- **WHEN** a developer clones the repo, enters the devenv shell, and runs the studio build
- **THEN** the build succeeds with no manually installed tools

#### Scenario: CI gates on studio health

- **WHEN** a PR breaks an Elm type or a studio test
- **THEN** CI fails on the studio job while reporting the Haskell jobs independently

