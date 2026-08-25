## 1. Add guarded `cabal update` to `ci:build` and `ci:release-build` tasks

- [x] 1.P Plan: Prepend a guarded `cabal update` to the `ci:build` and `ci:release-build` task exec strings in `devenv.nix` so dependency resolution works on a clean-slate runner (no `~/.cabal` cache). The guard MUST fail the task only when `cabal update` fails AND no cached Hackage index exists. Check criteria: (1) `devenv tasks run ci:build` succeeds on a clean slate; (2) `zip-archive` resolves to a Hackage version during `cabal configure`; (3) warm-cache runs are unaffected; (4) `ci:release-build` gets the same guard. Affected: `devenv.nix`. Risk: shell quoting inside the Nix multi-line string.
- [x] 1.D Do: Edit `devenv.nix` `tasks."ci:build".exec` and `tasks."ci:release-build".exec` to: `cabal update || { test -d ~/.cabal/packages/hackage.haskell.org && echo "cabal update failed; using cached index"; } || { echo "cabal update failed and no cached index"; exit 1; } && cabal configure ... && cabal build all -j4`. Preserve existing `cabal configure` flags (`--enable-tests --flag dev -j4` for `ci:build`, plain for `ci:release-build`). Add an inline comment explaining why `cabal update` is required (clean-slate CI has no Hackage index).
- [x] 1.C Check: (1) Re-run the failing GitHub Actions `Build` step — confirm `zip-archive` resolves and the build reaches GHC compilation. (2) Locally: `rm -rf ~/.cabal/packages/hackage.haskell.org` inside a nix-shell, then `devenv tasks run ci:build` — confirm `cabal update` repopulates the index and the build proceeds. (3) Run `devenv tasks run ci:build` a second time on the warm cache — confirm success and no behavior change. (4) Verify `ci:release-build` exec string now also starts with the guarded `cabal update`. (5) `openspec validate --changes --json` passes for this change.
- [x] 1.A Act: If CI still fails, inspect the `cabal update` output — if Hackage is down, pin an `index-state` in `graphos.cabal` as a follow-up change. If the guard's shell quoting is wrong inside the Nix string, switch to a dedicated `ci:prepare` task (design D1 alternative A). If all checks pass, mark done.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Verify the `devenv-shell` spec delta captures the invariant

- [x] 2.P Plan: Confirm the ADDED requirement in `specs/devenv-shell/spec.md` (this change) correctly states that `ci:build` SHALL run `cabal update` before `cabal configure`, with scenarios for clean-slate and warm-cache runs. Check criteria: (1) `openspec validate --changes --json` passes; (2) the delta uses `## ADDED Requirements` (not MODIFIED for the new requirement); (3) every scenario uses exactly 4 hashtags (`####`). Affected: `openspec/changes/fix-ci-missing-hackage-package-list/specs/devenv-shell/spec.md`. Risk: using 3 hashtags for scenarios silently fails validation.
- [x] 2.D Do: Review `specs/devenv-shell/spec.md`. Ensure the new requirement ("CI build task refreshes the Hackage package index before configure") is under `## ADDED Requirements`, uses SHALL/MUST, and each `#### Scenario:` has WHEN/THEN. Ensure the unchanged baseline requirement is under `## MODIFIED Requirements` only if its content changed; otherwise leave the baseline alone and rely on the ADDED block. Run `openspec validate --changes --json` and fix any schema errors.
- [x] 2.C Check: (1) `openspec validate --changes --json` returns valid with no errors for this change. (2) `grep -c "^#### Scenario:" specs/devenv-shell/spec.md` returns at least 4 (the new requirement's scenarios). (3) The ADDED requirement text explicitly mentions `cabal update` before `cabal configure`. (4) No scenario uses 3 hashtags.
- [x] 2.A Act: If validation reports a schema error, fix the headers (3→4 hashtags, ADDED vs MODIFIED). If the baseline requirement was incorrectly placed under MODIFIED without changes, remove the MODIFIED block to avoid losing detail at archive time. Mark done when validation passes.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Regression: confirm CI build passes end-to-end on a clean runner

- [x] 3.P Plan: Confirm the full CI workflow (`devenv tasks run ci:build && devenv tasks run ci:test`) passes on a clean-slate runner after the fix, and that no `unknown package` error appears. Check criteria: (1) GitHub Actions `Build` step succeeds; (2) `Run tests` step (`devenv tasks run ci:test`) succeeds; (3) no `unknown package` or `Could not resolve dependencies` error in logs. Affected: CI only (no code change). Risk: `ci:test` depends on `ci:build@succeeded` — if build is flaky, test step is skipped.
- [x] 3.D Do: Push the branch / open a PR triggering the `Haskell CI` workflow. Monitor the `build-and-test` job. If the job was previously failing on `Build`, confirm it now passes `Build` and proceeds to `Run tests`. Collect the `cabal configure` log excerpt showing `zip-archive` resolved to a version.
- [x] 3.C Check: (1) The `Build` step (`devenv tasks run ci:build`) exits 0. (2) The `Run tests` step (`devenv tasks run ci:test`) exits 0. (3) No `unknown package` string in the CI logs. (4) The OpenSpec validation step continues to run (it is `continue-on-error: true`, so its status is informational only).
- [x] 3.A Act: If `Build` passes but `Run tests` fails, that's a separate issue (not this change's scope) — report it but do not block this change. If `Build` still fails on `unknown package`, the guard from task 1 is wrong — reopen task 1. If all green, mark this change as verified and ready to archive.

### Attempt history (3)

<!-- empty unless a retry is needed -->