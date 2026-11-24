## Why

Graphos' frontend has grown into a substantial, independently-tooled codebase — an Elm SPA (`studio/`, its own `elm.json`, 55 elm-tests, dedicated CI) and a static HTML viewer (`assets/viewer/`) — yet it lives inside the Haskell monorepo, so it inherits the Haskell repo's release cadence, review load, and `cabal`-centric tooling. Splitting the frontend into its own `graphos-studio` repository gives it an independent build, test, release, and ownership cycle, shrinks the Haskell repo's surface, and lets the two halves version and deploy on their own schedules while they talk only over the existing `graph-json-contract` and `graphos serve` HTTP API.

## What Changes

- Create a new repository **`graphos-studio`** owning the frontend.
- Move `studio/` (the Elm Graphos Studio SPA) into `graphos-studio` — source, `elm.json`, tests, `.scripts`, `public/`, and its `studio.yml` CI.
- Move `assets/viewer/` (the `json-graph-web-view` HTML viewer) into `graphos-studio`. **BREAKING**: the Haskell binary stops embedding the viewer at compile time via `file-embed`; instead it is **served separately at runtime** from the frontend repo or a published bundle.
- Move `openspec-view/` (the OpenSpec artifact viewer) into `graphos-studio`. **BREAKING**: it is currently a `base`-only sub-library in `graphos.cabal` on which the main `graphos` library and the `view` subcommand depend; extraction decouples that dependency.
- Update `graphos.cabal` (drop `openspec-view` sub-library and the `file-embed` viewer assets), `devenv.nix` (drop the Elm toolchain), and `.github/workflows/haskell.yml` (replace the in-repo `studio-contract` job with a cross-repo contract check against `graphos-studio`).

## Capabilities

### New Capabilities

- `frontend-repository`: The `graphos-studio` repository — its layout, Elm build + test toolchain, the published studio bundle and viewer assets it produces, and the serving contract that `graphos serve` consumes so a user can still open the studio/viewer against a running Graphos server.

### Modified Capabilities

- `html-viewer-assets`: The requirement that viewer CSS/JS are source files **embedded at compile time** into the Haskell binary is removed; the viewer is instead built and served from `graphos-studio` and fetched/embedded at runtime.
- `studio-app-shell`: The studio bundle is no longer produced inside the Haskell repo by `devenv`; it is built in `graphos-studio` and `graphos serve` resolves the bundle from the frontend repo or a published location rather than the local `studio/public`.

## Impact

- **Code moved out**: `studio/`, `assets/viewer/`, `openspec-view/`, and `studio/.scripts/`.
- **Haskell build**: `graphos.cabal` loses the `openspec-view` sub-library, its test suite, the `file-embed` dependency usage, and the `extra-source-files` viewer entries; `HTML.hs` stops calling `embedFile` for viewer assets.
- **CLI**: the `view` subcommand currently backed by `openspec-view` must be re-wired (external dependency, re-import, or removed) once that sub-library leaves the repo.
- **CI**: `studio.yml` moves to `graphos-studio`; `haskell.yml`'s `studio-contract` job (which runs `generate-contract-fixtures.sh` + `elm-test` against the studio decoders) becomes a cross-repo check.
- **Dev shell**: `devenv.nix` drops `elm`, `elm-test`, `elm-format`, `elm-review`.
- **Shared contract**: `graph-json-contract` and the `graphos serve` slice/query API become the cross-repo boundary; both are unchanged but now owned jointly.
