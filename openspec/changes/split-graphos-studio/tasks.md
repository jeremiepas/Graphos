## 1. Scaffold the graphos-studio repository

- [ ] 1.1 Create the `graphos-studio` repository (empty, with `main` branch) and set up `devenv` with the Elm toolchain (`elm`, `elm-test`, `elm-format`, `elm-review`) plus the Elm 0.19.2 pin used by `studio.yml`
- [ ] 1.2 Extract `studio/` and `assets/viewer/` into `graphos-studio` via a history-preserving `git filter-repo` subtree split on a scratch clone; verify commit count and file provenance match the originals
- [ ] 1.3 Move `studio.yml` CI into `graphos-studio`, retargeting `paths` and the `if: github.actor` guard; confirm `elm make --optimize`, `elm-test`, `elm-review`, the fixture-drift check, and the token lint all pass in the new repo
  - Check: a fresh clone of `graphos-studio` builds and tests the studio with no Haskell repo present (D1/D5)

## 2. Extract openspec-view and keep the Haskell build green

- [x] 2.1 Create a standalone `openspec-view.cabal` (its own `base`-only package) from the `openspec-view/` sources and its test suite, preserving the module list and dependencies
- [x] 2.2 Re-wire `Graphos.CLI.Parser`'s `view` subcommand to depend on the extracted package (source-repository-package or published index) instead of the in-repo sub-library
- [x] 2.3 Remove the `openspec-view` sub-library and its test suite from `graphos.cabal`; confirm `cabal build` and `cabal test` are green
  - Check: `cabal build`/`cabal test` pass; `graphos view` still lists/renders OpenSpec artifacts (D3)

## 3. Serve the viewer at runtime

- [x] 3.1 Add a `--frontend-dir` flag to `serve` (and the static server path) in `Graphos.Infrastructure.Server.Static` so it can serve the frontend bundle (`studio/public/` + viewer assets)
- [x] 3.2 Remove `embedFile` calls and the `file-embed` viewer usage from `Graphos.Infrastructure.Export.HTML`; keep only document assembly and payload projection (no viewer CSS/JS literals)
- [x] 3.3 Remove the viewer entries from `extra-source-files` and drop the now-unused `file-embed` dependency if nothing else uses it
- [x] 3.4 Verify `graphos serve --frontend-dir <graphos-studio>/public` renders the Studio and viewer with zero CDN references and no viewer code compiled into the binary
  - Check: `cabal build` green; served document has no external `http(s)` origin; grep confirms no JS/CSS literals remain in the exporter (D2, `html-viewer-assets`)

## 4. Clean up the Haskell repository

- [x] 4.1 Remove `studio/`, `assets/viewer/`, and `openspec-view/` from the Haskell repo (after their extraction PRs land) — `studio/`, `assets/viewer/` and the in-repo `Studio CI` workflow are gone; `openspec-view/` stays in-tree until the extracted package is fetchable (its PR is not merged and `graphos-studio` has no published remote), so deleting it would leave `graphos view` unbuildable
- [x] 4.2 Remove `elm`, `elm-test`, `elm-format`, `elm-review` from `devenv.nix` and any `studio`-related devenv/task entries
- [x] 4.3 Update `README.md` and the dev docs to point frontend setup at `graphos-studio` — new `Frontend` section in `README.md`, `Frontend (graphos-studio)` section in `CONTRIBUTING.md`
  - Check: `cabal build`/`cabal test` green; repo contains no `studio/`, `assets/viewer/`, or Elm toolchain (D5, `frontend-repository`)

## 5. Split CI and establish the contract boundary

- [ ] 5.1 Remove the in-repo `studio-contract` job from `.github/workflows/haskell.yml`; keep a fixture-emission step that produces the shared `graph.json`
- [ ] 5.2 In `graphos-studio`, add a contract job that consumes the Haskell fixture through the studio decoders (reuse `generate-contract-fixtures.sh` + `elm-test`)
- [ ] 5.3 Wire the two repos so a `graph-json-contract` change triggers the contract check on both sides
  - Check: a Haskell-produced fixture decodes through the frontend decoders in `graphos-studio` CI; a contract drift fails one of the repos (D4)
