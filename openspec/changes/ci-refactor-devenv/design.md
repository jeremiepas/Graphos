## Context

Graphos CI currently uses two GitHub Actions workflows (`haskell.yml` and `release.yml`) that manually install GHC 9.10 and cabal via `haskell-actions/setup@v2`, then run cabal commands step-by-step. The local dev environment uses `devenv.nix` which declares GHC 9.10, cabal, system dependencies, and tooling in a single Nix file. These two definitions drift apart — GHC versions, flags, and dependency lists must be maintained in both places.

The project already has a `devenv.nix` with `languages.haskell` configuration and a `scripts.mgconsole` entry, but no `tasks` block and no `cloud.ci.github` config. devenv's task system (`tasks`, `devenv tasks run`, `devenv test`) provides a way to define build/test/CI operations declaratively and run them identically locally and in CI.

## Goals / Non-Goals

**Goals:**
- Define all CI operations as devenv tasks so `devenv tasks run ci:test` works identically locally and in CI
- Replace manual cabal steps in GitHub Actions with `devenv tasks run` invocations
- Use Cachix for Nix store caching (replaces manual cabal cache actions)
- Ensure GHC version, flags, and dependencies come from a single source: `devenv.nix`

**Non-Goals:**
- Changing the application code, domain, or use-case layers (infrastructure-only change)
- Adding new CI jobs beyond what exists today (build+test, haddock, release)
- Migrating to flake-based Nix (devenv.yaml already pins nixpkgs-unstable)
- Setting up Cachix binary cache for the project's own packages (only using the public Cachix cache for Nix dependencies)

## Decisions

### Decision 1: Use devenv tasks as the CI primitive (not `devenv shell` with inline commands)

**Alternatives considered:**
- A: Use `devenv shell -- cabal build all` in each CI step
- B: Define `tasks` in `devenv.nix` and use `devenv tasks run ci:test`

**Choice: B** — Tasks are declarative, self-documenting, have dependency ordering (`after`/`before`), and can be run locally with `devenv tasks run <name>`. Inline `devenv shell` commands would still scatter CI logic across YAML files.

### Decision 2: Use `cachix/install-nix-action` + `cachix/cachix-action` + `nix profile add nixpkgs#devenv`

**Alternatives considered:**
- A: Use `cachix/devenv-action` (a convenience wrapper)
- B: Use the documented install steps: install-nix-action → cachix-action → nix profile add devenv

**Choice: B** — This is the officially documented approach from devenv's GitHub Actions integration guide. It gives explicit control over Nix and Cachix versions and is more transparent for debugging.

### Decision 3: Keep `haskell.yml` and `release.yml` as separate workflows

**Alternatives considered:**
- A: Merge into a single workflow with conditional jobs
- B: Keep two separate workflow files, both using devenv

**Choice: B** — Different triggers (push/PR vs tags), different permissions, different outputs. Merging adds complexity without benefit.

### Decision 4: Use `cloud.ci.github` config in devenv.nix for branch-aware task logic

**Rationale:** The `cloud.ci.github` config exposes `branch`, `ref`, and `base_ref` from the GitHub Actions environment. This allows tasks to conditionally run steps (e.g., haddock only on main branch pushes). This is a built-in devenv feature documented in their cloud module.

### Decision 5: Task naming convention: `ci:` prefix

All CI tasks use the `ci:` namespace (e.g., `ci:build`, `ci:test`, `ci:haddock`). This keeps them visually distinct from development tasks and makes it easy to list all CI tasks with `devenv tasks run ci:`.

## Risks / Trade-offs

- **[Risk] Nix build cold start slower than haskell-actions/setup** → The first CI run will be slower as Nix builds the shell. Mitigation: Cachix caches the Nix store, so subsequent runs reuse cached derivations. After warm-up, Nix builds are competitive.
- **[Risk] devenv tasks run may change behavior across versions** → Pin devenv version via `devenv.lock` (already present). The lock file pins exact Nixpkgs revision.
- **[Risk] Debugging Nix failures in CI is harder than plain cabal** → Mitigation: keep `devenv shell -- cabal <cmd>` as a fallback. Task definitions map 1:1 to cabal commands, so fallback is trivial.
- **[Trade-off] Slightly more complex CI setup** → In exchange for single-source-of-truth for all build dependencies and commands.

## Verification Strategy (Check)

1. `devenv tasks run ci:build` succeeds locally (builds all targets with `-j4`)
2. `devenv tasks run ci:test` succeeds locally (runs full test suite)
3. `devenv tasks run ci:haddock` succeeds locally (generates documentation)
4. `devenv tasks run ci:release-build` succeeds locally (builds release binary)
5. GitHub Actions `haskell.yml` workflow passes on push to `main`
6. GitHub Actions `haskell.yml` workflow passes on pull request to `main`
7. GHC version in CI matches `devenv.nix` (9.10) — verify via `ghc --version` in CI logs
8. No manual GHC/cabal version environment variables remain in workflow YAML

## Iteration & Rollback (Act)

- **If CI fails after migration**: Revert workflow YAML files to the previous `haskell-actions/setup` approach. The `devenv.nix` tasks remain and are still useful locally. Git revert on the two YAML files is the rollback.
- **If Nix caching is insufficient**: Increase Cachix cache scope or add a self-hosted runner with persistent Nix store.
- **Learnings to standardize**: After successful migration, any new CI operation (linting, benchmarking) is added as a `ci:` task in `devenv.nix` only — no YAML editing needed for the command itself.