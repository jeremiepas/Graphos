## Context

Graphos builds via Cabal inside a `devenv` shell. The CI workflow (`.github/workflows/haskell.yml`) runs `devenv tasks run ci:build`, which executes the `ci:build` task defined in `devenv.nix`:

```nix
"ci:build" = {
  exec = ''cabal configure --enable-tests --flag dev -j4 && cabal build all -j4'';
};
```

There is no `cabal update` anywhere in the pipeline. On a clean GitHub Actions runner (or any fresh nix-shell without a populated `~/.cabal`), the Hackage package list does not exist. Cabal therefore cannot resolve any dependency — it reports `unknown package: zip-archive (dependency of graphos)` because `zip-archive` is the first dependency it tries that isn't bundled with GHC. Any other newly-declared Hackage dependency would produce the same class of error.

Locally the build appears to work only because earlier `cabal update` invocations (from manual dev usage) populated `~/.cabal/packages/hackage.haskell.org/` on the developer's machine.

Key constraints:
- The fix must not change the dependency set declared in `graphos.cabal` — `zip-archive` is legitimately needed by `Graphos.Infrastructure.FileSystem.OfficeConvert`.
- CI runs on `ubuntu-latest` with `cachix/install-nix-action` and `cachix/cachix-action` (devenv cache). The nix-provided `cabal-install` starts with no `~/.cabal` config.
- The `devenv tasks` runner executes the `exec` string in `bash -c`, so shell composition (`||`, `&&`) is available.

## Goals / Non-Goals

**Goals:**
- Make `devenv tasks run ci:build` pass on a clean-slate runner with no cached cabal state.
- Ensure dependency resolution (the `cabal configure` step) never fails due to a missing/stale Hackage index.
- Codify the "update before configure" invariant in the `devenv-shell` spec so future task definitions don't regress.

**Non-Goals:**
- Changing which Haskell dependencies `graphos.cabal` declares (no add/remove/pin of `zip-archive` or any other package).
- Migrating from Cabal to Stack or Nix-only builds (Cabal remains the build tool).
- Pinning a specific Hackage `index-state` for bit-for-bit reproducible dependency resolution (open as a follow-up in the Act phase, not this change).
- Fixing the OpenSpec validation step (`openspec validate --changes --json`) — it already has `continue-on-error: true`.

## Decisions

### D1: Prepend `cabal update` to the `ci:build` exec string

**Decision**: Change the `ci:build` task to:
```nix
"ci:build" = {
  exec = ''cabal update && cabal configure --enable-tests --flag dev -j4 && cabal build all -j4'';
};
```

**Alternatives considered**:
- A: Add a separate `devenv` task (`ci:prepare`) that runs `cabal update` and make `ci:build` depend on it via `after = [ "ci:prepare@succeeded" ]`. — Cleaner separation, but adds task-graph complexity for a one-line fix and the `ci:build` task is the only consumer.
- B: Run `cabal update` in the GitHub Actions workflow before `devenv tasks run ci:build`. — Pushes build-internal logic into the workflow YAML, duplicating it for local `devenv tasks run ci:build` invocations. Diverges from "devenv tasks are the single source of truth for CI."
- C: **Prepend `cabal update` inline in the `ci:build` exec.** — Keeps the fix co-located with the build definition, applies identically in CI and locally, minimal diff.

**Rationale**: The `devenv.nix` tasks block is already the single source of truth for what CI runs (the workflow just calls `devenv tasks run ci:build`). Putting `cabal update` there means local `devenv tasks run ci:build` and CI behave identically, and the fix is one line in the file that already owns this concern.

**Layer**: `devenv.nix` (build/CI infrastructure), `devenv-shell` spec (contract).

### D2: Best-effort `cabal update` to avoid flakiness on warm caches

**Decision**: Use a guarded `cabal update` so a transient Hackage/network failure does not fail the build when a usable index already exists:

```nix
"ci:build" = {
  exec = ''cabal update || { test -d ~/.cabal/packages/hackage.haskell.org && echo "cabal update failed; using cached index"; } || { echo "cabal update failed and no cached index"; exit 1; } && cabal configure --enable-tests --flag dev -j4 && cabal build all -j4'';
};
```

**Alternatives considered**:
- A: Plain `cabal update && cabal configure ...` — simplest, but any Hackage hiccup fails CI even when a perfectly good cached index exists. Historically Hackage has occasional 503s.
- B: Retry loop (`for i in 1 2 3; do cabal update && break; sleep 5; done`). — More robust against transient failures, but adds shell complexity and hides persistent failures behind retries.
- C: **Guarded fallback** — fail only when `cabal update` fails AND no cached index exists. On a true clean slate (the failing case we're fixing) there is no cached index, so a `cabal update` failure correctly fails the task. On a warm cache, a transient Hackage error degrades gracefully.

**Rationale**: The actual bug is "no index at all" (clean slate), not "stale index." The guard precisely targets that: clean-slate failure is fatal (correct), warm-cache transient is non-fatal (avoids CI flakiness).

**Layer**: `devenv.nix` `ci:build` exec.

### D3: Codify the invariant in the `devenv-shell` spec

**Decision**: Add an ADDED requirement to the existing `devenv-shell` spec stating that the `ci:build` task SHALL run `cabal update` before `cabal configure`, with scenarios for clean-slate and warm-cache runs.

**Alternatives considered**:
- A: Don't update the spec — just fix the code. — Future task edits could silently drop `cabal update` and reintroduce the bug; the spec is the regression contract.
- B: **Add a spec requirement** — makes the invariant enforceable via `openspec validate` and visible to anyone editing `devenv.nix`.

**Rationale**: The `devenv-shell` spec already owns the CI task contract (it codifies GHC 910 tooling, `mgconsole`, etc.). The "update before configure" rule belongs there.

**Layer**: `openspec/specs/devenv-shell/spec.md` (via delta in this change's `specs/devenv-shell/spec.md`).

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| `cabal update` adds ~5-15s to cold CI runs | One-time cost per run; cachix/devenv caching of the nix shell is unaffected. Acceptable for CI. |
| Hackage transient failures flake CI | D2's guarded fallback makes warm-cache runs tolerant of transient `cabal update` failures. |
| `cabal update` pulls a newer index than the dev machine, causing version drift | `graphos.cabal` uses lower bounds (`>=`) not exact pins, so a newer compatible patch release is fine. If it becomes a problem, follow-up Act: pin `index-state` in `graphos.cabal`. |
| Guarded shell logic is slightly harder to read | Inline comment in `devenv.nix` explains the intent; the spec scenario documents the behavior. |
| Future task definitions copy `ci:build` without `cabal update` | D3 spec requirement makes it enforceable; `openspec validate` catches regressions at PR time. |

## Verification Strategy (Check)

1. **Clean-slate CI reproduction**: Re-run the failing GitHub Actions job. Confirm `zip-archive` resolves and the build reaches GHC compilation (no `unknown package` error). This is the primary acceptance gate.
2. **Local clean-slate simulation**: Inside a fresh nix-shell, move/empty `~/.cabal/packages`, run `devenv tasks run ci:build`. Confirm `cabal update` populates the index and the build proceeds.
3. **Warm-cache regression**: Run `devenv tasks run ci:build` twice in a row on a machine with a populated index. Confirm the second run is unaffected (build still succeeds, no behavior change).
4. **Transient-failure simulation**: Temporarily break `cabal update` (e.g., `CABAL_CONFIG` pointing at a bad mirror) on a warm cache and confirm the build still succeeds via the fallback (D2). Then restore.
5. **Spec validation**: `openspec validate --changes --json` passes for this change; the `devenv-shell` delta is well-formed.

## Iteration & Rollback (Act)

- **If CI still fails after the fix**: Check the actual `cabal update` output. If Hackage itself is down, the guard (D2) should keep warm caches working; if it's a clean-slate run during a Hackage outage, pin an `index-state` in `graphos.cabal` as a follow-up.
- **If dependency drift from newer indices breaks the build**: Add `index-state: 2026-08-11T00:00:00Z` (or the relevant date) to `graphos.cabal` for reproducible resolution. Track as a separate change.
- **If the guarded shell logic proves fragile**: Replace D2 with a small shell function or a dedicated `ci:prepare` task (alternative A from D1) for clearer structure.
- **Standardize**: The "update before configure" rule is now in the `devenv-shell` spec. Future CI task additions (e.g., a `ci:lint` task) that run `cabal configure` must follow the same invariant.

## Migration Plan

1. Edit `devenv.nix` `tasks."ci:build".exec` to prepend the guarded `cabal update` (D1+D2).
2. Commit the `devenv-shell` spec delta (already in this change).
3. Push to `main` / open PR — the CI build step is the verification (it was failing before).
4. Rollback: Revert the `devenv.nix` one-liner. The spec delta is non-functional and can stay or revert together. No database, config, or user-facing migration is involved.

## Open Questions

- Should we pin a Hackage `index-state` in `graphos.cabal` for full reproducibility? **Deferred** — out of scope for this fix; revisit in Act if version drift appears.
- Should `cabal update` run in `ci:release-build` too? **Yes, by symmetry** — `ci:release-build` currently also omits `cabal update` and has the same latent bug. Folded into the implementation task (see tasks.md).