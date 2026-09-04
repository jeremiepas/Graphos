## Context

Three workflows touch the graphos binary:

- `haskell.yml` (push to main): install-nix → Cachix(devenv) → `devenv tasks
  run ci:build` / `ci:test`. Only the nix store is cached (Cachix); the
  cabal store (`~/.cabal/store` + `~/.local/state/cabal`, layout depends on
  cabal-install) and `dist-newstyle` are rebuilt every run, then discarded.
- `graphos-analyze.yml` (dispatch, per-repo matrix): downloads a release
  binary for `graphos_version`; the repo currently has **zero releases**,
  so every matrix job runs the devenv source build fallback — the same
  multi-minute build once per analyzed repo.
- `release.yml` (tags): the only path that publishes a binary today.

Repo conventions that constrain the design:

- CI operations are **devenv tasks** (`ci:build`, `ci:test`, `ci:haddock`,
  `ci:release-build`, `ci:release-test`) — the tasks are the single source
  of truth, YAML only invokes them (archived change `ci-refactor-devenv`).
- Cachix `devenv` cache is used read-only (no `CACHIX_AUTH_TOKEN` in the
  workflows) — it substitutes the nix side only.
- The analyzer's read-only contract (graph outputs, metrics, artifacts)
  is fixed; only *how the binary is obtained* changes.

## Goals / Non-Goals

**Goals:**

- Stop rebuilding the cabal store on every push (cache it).
- Stop discarding the CI binary (publish it as a workflow artifact).
- Make the analyzer prefer prebuilt binaries (release → artifact) and
  only build from source as a last resort.
- Keep everything inside the existing conventions: new operation is a
  devenv task; workflows only orchestrate tasks.

**Non-Goals:**

- Publishing releases from CI (releases stay tag-driven via `release.yml`).
- Pushing the cabal store to Cachix (would need a signing token/secret;
  `actions/cache` covers the need without secrets).
- Cross-platform or non-x86_64 binaries (the analyzer runs on
  ubuntu-latest x86_64; the release asset name `graphos-linux-x86_64` is
  the existing convention).
- Changing the analyzer's analysis semantics, flags, or outputs.

## Decisions

- **Cabal cache via `actions/cache`, not Cachix.**
  `actions/cache@v4` over the cabal store directory and `dist-newstyle`,
  key: `cabal-<runner.os>-ghc910-<hashFiles(**/*.cabal, cabal.project)>`,
  restore-keys prefix-fallback so lockfile-adjacent churn still restores
  most of the store. The store path differs by cabal version; we detect
  it at runtime (`~/.cabal/store` if present, else
  `~/.local/state/cabal/store`) rather than hardcoding, and `dist-newstyle`
  is at the repo root.
  - *Alternative considered:* Cachix with an auth token — rejected: needs
    a stored secret and signs nix-style, not cabal's layout.
- **New devenv task `ci:bin` (verify + print binary path).**
  `exec` runs `cabal list-bin graphos`, validates the file exists, and
  echoes its path — failing with a clear message when the binary has not
  been built. Caveat discovered during implementation: `devenv tasks run`
  does not pass task stdout through as capturable process output (it
  prints `{}` to stdout; task stdout is only visible with
  `--show-output`), so workflows capture the path with
  `devenv shell -- cabal list-bin graphos` and run `ci:bin` as a fail-fast
  preflight (it fails non-zero when the binary is missing). The task
  still follows the tasks-as-source-of-truth convention for *checking*;
  the shell one-liner is kept because it is capturable.
- **Artifact name and contents:** single workflow artifact `graphos-bin`
  containing one file `graphos-linux-x86_64` (the executable, `chmod +x`
  before upload). Name matches the release asset so downstream consumers
  treat both identically. Upload only after `ci:test` succeeds
  (`if: success()` on a post-test step) — a broken binary must never be
  published as reusable. Retention: `retention-days: 14` — enough to cover
  the "latest successful main build" window without hoarding.
- **Analyzer acquisition order (job-level, resolved once per matrix job
  or hoisted to a prepare step):**
  1. *Release asset* — if `graphos_version` is `latest`: GET
     `releases/latest`, else GET `releases/tags/<version>`; if the release
     exists **and** has asset `graphos-linux-x86_64`, download it. 404s
     are the normal "no release yet" path, not errors.
  2. *CI artifact* — list workflow runs for `haskell.yml` on `main` with
     `status=success`, take the newest with a `graphos-bin` artifact, and
     download it. Requires `actions: read` (and `contents: read`) on the
     workflow's `permissions:` block; uses the default `GITHUB_TOKEN`
     (no stored secrets). The GitHub CLI (`gh`) is preinstalled on
     ubuntu-latest runners and handles pagination/auth: `gh run list
     --workflow haskell.yml --branch main --status success --limit N`,
     then `gh run download <id> --name graphos-bin`.
  3. *Source build* — unchanged existing fallback: nix → Cachix →
     `nix profile add nixpkgs#devenv` → `devenv tasks run ci:build` →
     `ci:bin` → copy binary. Kept verbatim so the analyzer never breaks
     even with no releases, no prior CI runs, and cold caches.
- **The nix/devenv install steps become conditional (`if: steps.bin.outputs.found != 'true'`).**
  When a prebuilt binary is found, the job skips install-nix + Cachix +
  devenv entirely (saves ~1–2 min toolchain bootstrap + several minutes
  of cabal build per matrix job). The checkout step stays unconditional
  (cheap, and needed by the fallback path).
- **Step shape for acquisition:** a single `id: getbin` step that
  implements the order and sets `outputs.found` (`release`|`artifact`|``)
  plus `outputs.path` (local binary path). Shell-only, `set +e` around
  network probes so failures fall through rather than aborting; only the
  final fallback build failure fails the job.
- **Permissions:** `graphos-analyze.yml` adds `actions: read` to its
  `permissions: contents: read` block — the minimum for listing workflow
  runs/artifacts of the same repo. `haskell.yml` needs nothing new
  (artifact upload within the same run works with the default token).

## Risks / Trade-offs

- [Artifact staleness] The CI artifact reflects the last successful
  `main` build, not the dispatched commit. Accepted: the analyzer
  explicitly requests a version ("latest release" → artifact → source);
  users needing an exact commit can rely on the release path or the
  source fallback. Documented in the workflow input description.
- [Cache growth] `actions/cache` evicts by LRU (10 GB repo quota); the
  cabal store of this project is well under that. Restore-keys keep hit
  rates high across dependency bumps (partial restore + incremental
  rebuild).
- [`gh` availability] Preinstalled on GitHub-hosted ubuntu runners; if it
  ever disappears, the step falls through to the source build — graceful.
- [Same-run artifact not visible] The analyzer is dispatch-only; it never
  races `haskell.yml` in the same run, so "latest successful main" is
  always a *previous* run — no ordering hazard.
- [Release API rate limits] Unauthenticated probes are avoided by using
  the workflow token via `gh` for both release and artifact lookups.

## Migration Plan

- Additive: one devenv task, one spec'd cache+artifact block in
  `haskell.yml`, one acquisition step + permissions tweak in
  `graphos-analyze.yml`, `release.yml` switches to `ci:bin`.
- Rollback: revert the workflow diffs; the `ci:bin` task is inert if
  unused. No code or data-format changes.
- Verification: dispatch the analyzer (expect artifact or source path
  green), push a trivial change to main (expect cache hit + artifact
  upload), inspect the run's cache report and artifact listing.