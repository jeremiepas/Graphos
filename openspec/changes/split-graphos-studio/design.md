## Context

Graphos is a Haskell monorepo (`cabal`, `devenv`) whose frontend has three pieces: the Elm Studio (`studio/`, standalone `elm.json` + CI), the static HTML viewer (`assets/viewer/`, embedded into the binary via `file-embed` in `Graphos.Infrastructure.Export.HTML`), and the `openspec-view` Haskell sub-library (a `base`-only library in `graphos.cabal` on which the main `graphos` library and the `view` subcommand depend). The Studio already connects to `graphos serve` over the `graph-json-contract` shape and the slice/query API, so a clean HTTP boundary already exists. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Extract the frontend into a standalone `graphos-studio` repository with its own build, test, and CI.
- Remove compile-time viewer embedding from the Haskell binary; serve the viewer at runtime via `graphos serve`.
- Decouple the `openspec-view` sub-library from the main `graphos` library and CLI.
- Preserve the `graph-json-contract` boundary so the two repos cannot silently drift.

**Non-Goals:**

- Changing the Studio's feature set, navigation semantics, or data-source resolution behavior.
- Changing the `graph-json-contract` shape or the `graphos serve` slice/query API.
- Reintroducing a `develop` GitFlow branch or any CDN-based viewer loading.

## Decisions

### D1 — Extract the frontend with history-preserving subtree splits

Extract `studio/` and `assets/viewer/` into `graphos-studio` using `git filter-repo` subtree splits so file history survives the move. `openspec-view/` is extracted as a separate standalone package (see D3).

- **Alternatives considered**: (a) fresh `git init` + copy — loses history, rejected; (b) `git subtree` keep-in-sync — keeps coupling, rejected; (c) `git mv` within the same repo — doesn't create a separate repo, rejected.

### D2 — Serve the viewer at runtime; remove `file-embed`

The `Graphos.Infrastructure.Export.HTML` module stops calling `embedFile` for `viewer.css`/`viewer.js`/`vis-network.min.js`. The `Graphos.Infrastructure.Server.Static` server gains the ability to serve a frontend bundle directory (the Studio's `public/` output and the viewer assets) resolved from a configurable location (a `--frontend-dir` flag defaulting to a published `graphos-studio` bundle). Viewer assets move out of `extra-source-files` and out of the binary.

- **Alternatives considered**: (a) keep embedding via a git submodule — preserves self-containment but keeps a build-time coupling and a vendored copy, rejected per the "serve separately" decision; (b) fetch from a CDN — violates the existing "no external origin" requirement, rejected; (c) publish a versioned bundle the binary downloads at build time — still a compile-time coupling, rejected.

### D3 — Extract `openspec-view` as its own Haskell package

`openspec-view` becomes its own package (own `openspec-view.cabal` + repo, or published to a package index) and is re-imported as a normal dependency. The `view` subcommand in `Graphos.CLI.Parser` delegates to it through that dependency instead of the in-repo sub-library.

- **Alternatives considered**: (a) keep `openspec-view` in the Haskell repo (not moved) — contradicts the chosen scope, rejected; (b) inline the view command into the main library — duplicates logic, rejected; (c) drop the `view` subcommand — removes functionality, rejected.

### D4 — Preserve the contract boundary with a cross-repo contract check

The `graph-json-contract` spec and its `graph.json` fixtures become the shared contract. The Haskell repo keeps a job that emits a fixture; `graphos-studio` consumes that fixture through its decoders (the existing `generate-contract-fixtures.sh` + `elm-test` path) in its own CI. The in-repo `studio-contract` job in `haskell.yml` is removed in favor of the frontend repo's own contract gate.

- **Alternatives considered**: (a) duplicate the contract in both repos with no check — drift risk, rejected; (b) publish the contract as a generated schema artifact — more machinery than needed now, deferred.

### D5 — Split CI and the dev shell

`studio.yml` moves to `graphos-studio`; `devenv.nix` drops `elm`, `elm-test`, `elm-format`, `elm-review`. The Haskell repo's CI keeps only the Haskell jobs plus the fixture-emission step that feeds the frontend's contract check.

- **Alternatives considered**: (a) keep Elm toolchain in the Haskell devenv for convenience — reintroduces coupling, rejected; (b) run studio CI from the Haskell repo against the other repo — breaks the independent-ownership goal, rejected.

## Risks / Trade-offs

- **[Offline self-contained `graph.html` regression]** — the exported artifact was self-contained because the viewer was embedded; serving at runtime means the standalone `graph.html` is no longer fully offline. → Mitigation: document that interactive viewing goes through `graphos serve`; the frontend repo publishes a versioned self-contained bundle that `graphos serve` serves, keeping the "no CDN" property intact.
- **[Contract drift across repos]** — two repos can disagree on `graph-json-contract`. → Mitigation: the cross-repo contract check (D4) fails either repo when the fixture no longer decodes.
- **[`openspec-view` extraction breaks the `view` command]** — removing the sub-library mid-flight can break the build. → Mitigation: extract `openspec-view` as a publishable package and re-import it before deleting the in-repo sub-library; verify with `cabal build`.
- **[History-preserving split is fiddly]** — `git filter-repo` can drop or mangle history if misapplied. → Mitigation: run the split on a scratch clone, verify commit counts and file provenance, then open the PRs.

## Migration Plan

1. Scaffold the `graphos-studio` repository and extract `studio/` + `assets/viewer/` (D1).
2. Extract `openspec-view` as a package and re-wire the `view` subcommand (D3), keeping the Haskell build green.
3. Add frontend-bundle serving to `Server.Static` and remove `file-embed` from `Export.HTML` (D2).
4. Remove the viewer `extra-source-files`, the `openspec-view` sub-library, and the Elm toolchain from `devenv.nix`; move `studio.yml` and drop the in-repo `studio-contract` job (D5).
5. Set up the cross-repo contract check (D4).

**Rollback**: each step is independently revertible before its PR merges; the serving change (D2) is the only one that changes runtime behavior and can be rolled back by restoring the `file-embed` calls.

## Verification Strategy

- `cabal build` and `cabal test` green in the Haskell repo after the sub-library and asset removal (the clean-architecture boundary is unchanged: no Domain/UseCase edits).
- `graphos serve` renders the Studio and viewer from the frontend bundle with zero `file://` CDN references (D2 scenarios).
- The `graphos-studio` repo's `elm make --optimize`, `elm-test`, `elm-review` pass, and its contract job decodes the Haskell fixture (D4 scenarios).

## Open Questions

None — the serving location (configurable `--frontend-dir` defaulting to a published bundle) and the `openspec-view` extraction target (standalone package re-imported as a dependency) are resolved above.
