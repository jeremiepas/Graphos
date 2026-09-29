# Task 1 — Add guarded `cabal update` to `ci:build` and `ci:release-build` tasks — PLAN

**Task slug**: `01-add-guarded-cabal-update`
**Attempt**: 1
**Status**: pending

## Summary

Prepend a guarded `cabal update` to the `ci:build` and `ci:release-build` devenv task exec strings in `devenv.nix` so dependency resolution works on a clean-slate runner with no pre-existing `~/.cabal/packages/hackage.haskell.org/` directory. The guard uses the best-effort fallback pattern from design decision D2: fail only when `cabal update` fails AND no cached index exists; allow warm-cache runs to proceed even if `cabal update` has a transient error.

## Detail

### Scope

- Modify `devenv.nix` only — two task definitions:
  - `tasks."ci:build".exec` — prepend guarded `cabal update`, preserve existing `--enable-tests --flag dev -j4` flags
  - `tasks."ci:release-build".exec` — prepend guarded `cabal update`, preserve existing plain build flags
- Add an inline comment in the exec string explaining why `cabal update` is required (clean-slate CI has no Hackage index).
- No changes to `graphos.cabal`, no dependency changes, no workflow YAML changes.

### Check Criteria (defined BEFORE code)

| Criterion | Test/Gate | Spec Scenarios | PASS Condition |
|-----------|-----------|----------------|----------------|
| C1 | `devenv tasks run ci:build` on clean slate (after `rm -rf ~/.cabal/packages/hackage.haskell.org`) | `clean-slate-ci-run-resolves-all-dependencies` | `cabal update` downloads the index, `cabal configure` resolves all deps including `zip-archive`, build reaches GHC compilation, zero `unknown package` errors |
| C2 | `devenv tasks run ci:build` on warm cache (index already exists) | `warm-cache-run-is-unaffected` | Build succeeds, no behavior change vs. pre-fix, `cabal update` is idempotent |
| C3 | Simulated transient `cabal update` failure on warm cache (e.g., `CABAL_CONFIG` pointing to bad mirror) | `transient-hackage-failure-does-not-block-warm-cache-builds` | `ci:build` does NOT fail; `cabal configure` proceeds using cached index |
| C4 | `ci:release-build` task also contains the guarded `cabal update` prefix | *(implicit from task scope)* | Exec string starts with guarded `cabal update` pattern |
| C5 | No `unknown package` when a declared dependency truly doesn't exist | `missing-dependency-is-reported-clearly` | `cabal configure` fails with clear `unknown package: <name>` error, task exits non-zero |

**Exact test commands**:
```bash
# C1: Clean slate
nix-shell shell.nix --command 'rm -rf ~/.cabal/packages/hackage.haskell.org && devenv tasks run ci:build'
# Verify: exit 0, no "unknown package" in output

# C2: Warm cache
nix-shell shell.nix --command 'devenv tasks run ci:build'
# Run twice; confirm second run succeeds identically

# C4: Check ci:release-build exec string
grep -A1 '"ci:release-build"' devenv.nix | grep 'cabal update'
# Verify: contains the guarded cabal update pattern
```

**FAIL boundaries**:
- FAIL if `cabal update` fails AND `~/.cabal/packages/hackage.haskell.org` exists (warm cache should never fail on transient)
- FAIL if the exec string syntax is invalid (devenv can't parse the nix)
- FAIL if `--enable-tests --flag dev -j4` flags are lost from `ci:build`

### Affected Modules

| Module | Change |
|--------|--------|
| `devenv.nix` | Two exec strings modified (`ci:build`, `ci:release-build`) |

### Prerequisites

- Existing `devenv.nix` structure understood (tasks block, nix string quoting).
- `cabal` and `devenv` available in the nix shell.
- `zip-archive` declared as a dependency in `graphos.cabal`.

### Risks

| Risk | Mitigation |
|------|------------|
| Shell quoting inside Nix multi-line string breaks the exec | Use `''` (Nix multi-line) and standard shell `&&`/`||` — validated by testing the exec in a shell before committing |
| `cabal update` on clean slate takes 5–15s, slowing CI | One-time cost per run; acceptable per design |
| Transient Hackage 503 during a truly clean run | Guard requires cached index — on a real clean slate with no index, a Hackage outage correctly fails the build (expected behavior) |

### Dependency graph

- **No dependencies on other tasks** (foundation-level infrastructure fix).
- **Required before**: Task 3 (regression CI verification depends on Task 1's fix being in place).
