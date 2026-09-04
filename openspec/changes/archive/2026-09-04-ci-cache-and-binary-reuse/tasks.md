## 1. devenv task `ci:bin`

- [x] 1.1 Add a `ci:bin` task to `devenv.nix` that prints the built `graphos` executable path via `cabal list-bin graphos`, failing with a clear message when the binary has not been built yet
- [x] 1.2 Keep `release.yml` capturing the path via `devenv shell -- cabal list-bin graphos` (devenv task stdout is not capturable — see design.md caveat); `ci:bin` is used as a fail-fast preflight instead
- [x] 1.3 Local check: `devenv tasks run ci:bin` prints the existing binary path; in a clean env (or after `cabal clean`) it fails non-zero

## 2. Cabal store cache in `haskell.yml`

- [x] 2.1 Add an `actions/cache@v4` step to `haskell.yml` (before the build) covering the cabal store directory (runtime-detected: `~/.cabal/store` or `~/.local/state/cabal/store`) and `dist-newstyle`, keyed on `runner.os` + GHC version + `hashFiles('**/*.cabal', 'cabal.project')`, with a prefix restore-key for partial hits
- [x] 2.2 Keep the Cachix(devenv) step as-is (toolchain); verify the two caches compose (nix store hit + cabal store hit)
- [ ] 2.3 Verify on a push: run report shows a cache hit on an unchanged cabal definition, and a miss+save when `graphos.cabal` changes

## 3. Binary artifact in `haskell.yml`

- [x] 3.1 Add a post-test step in `haskell.yml` that runs `devenv tasks run ci:bin`, copies the binary to `dist/release/graphos-linux-x86_64`, `chmod +x` it, and uploads via `actions/upload-artifact@v4` as `graphos-bin` with `retention-days: 14`
- [x] 3.2 Gate the upload on build+test success only (no artifact for failing runs)
- [ ] 3.3 Verify on a push: the run lists a `graphos-bin` artifact containing exactly `graphos-linux-x86_64`, executable after download

## 4. Analyzer acquisition order in `graphos-analyze.yml`

- [x] 4.1 Add `actions: read` to the workflow `permissions:` block (alongside `contents: read`)
- [x] 4.2 Replace the unconditional "Get graphos binary" logic with an `id: getbin` step implementing the preference order: (a) release asset for the resolved `graphos_version` (or latest release when `latest`), via `gh release download` treating 404 as a soft miss; (b) latest successful `main` run's `graphos-bin` artifact via `gh run list --workflow haskell.yml --branch main --status success` + `gh run download`; (c) set `steps.getbin.outputs.found` and `outputs.bin` accordingly
- [x] 4.3 Make the nix/Cachix/devenv-install steps and the source build conditional on `steps.getbin.outputs.found != 'true'`, keeping the source fallback verbatim (nix → cachix → devenv → `ci:build` → `ci:bin` → copy)
- [x] 4.4 `chmod +x` any downloaded binary before use; fail the job with a clear message if the resolved file is missing/not executable
- [x] 4.5 Update the `graphos_version` input description to document the acquisition order (release → latest CI artifact → source build)

## 5. Verification

- [x] 5.1 `openspec validate ci-cache-and-binary-reuse` green; YAML parses (`python3 -c yaml.safe_load` or actionlint if available)
- [ ] 5.2 E2E (requires push): dispatch `graphos-analyze.yml` on a repo where no release exists but a prior main artifact does — assert the job log shows the artifact path (no nix/devenv steps) and analysis completes
- [ ] 5.3 E2E fallback (requires push): after clearing/ignoring artifacts (or a fresh matrix path), the source-build fallback still succeeds
- [ ] 5.4 E2E release (requires push): after the next tag/release with the asset, a dispatch resolves the release asset path
- [ ] 5.5 (requires push) Confirm analyzer outputs (graph.json keys, metrics, time-memory report) are unchanged versus the pre-change workflow on the same input repos