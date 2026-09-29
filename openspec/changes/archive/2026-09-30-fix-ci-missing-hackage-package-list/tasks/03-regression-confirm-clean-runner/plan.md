# Task 3 — Regression: confirm CI build passes end-to-end on a clean runner — PLAN

**Task slug**: `03-regression-confirm-clean-runner`
**Attempt**: 1
**Status**: pending

## Summary

Verify that the full CI workflow passes end-to-end after Task 1's fix: `devenv tasks run ci:build` succeeds on a clean-slate runner (no `unknown package` errors), and the subsequent test step (`devenv tasks run ci:test`) also succeeds. This is the primary acceptance gate — confirming the actual CI failure is resolved, not just that the code looks correct.

## Detail

### Scope

- No code changes — CI verification only.
- Trigger the GitHub Actions `Haskell CI` workflow by pushing the fix branch or opening a PR.
- Monitor the `build-and-test` job: `Build` step (Task 1's fix) and `Run tests` step (Task 1 + Task 2 together).
- Collect `cabal configure` log excerpt showing `zip-archive` resolved to a version.

### Check Criteria (defined BEFORE code)

| Criterion | Test/Gate | Spec Scenarios | PASS Condition |
|-----------|-----------|----------------|----------------|
| C1 | GitHub Actions `Build` step exits 0 | `clean-slate-ci-run-resolves-all-dependencies` | `devenv tasks run ci:build` completes with exit code 0, no `unknown package` or `Could not resolve dependencies` errors |
| C2 | GitHub Actions `Run tests` step exits 0 | *(implicit from ci:test task)* | `devenv tasks run ci:test` completes with exit code 0 |
| C3 | No `unknown package` string in CI logs | *(validation against C1 logs)* | `grep -i "unknown package" ci-logs` returns zero matches |
| C4 | OpenSpec validation step runs (informational) | *(from Task 2)* | `openspec validate --changes --json` runs and completes (has `continue-on-error: true`, so status is informational) |
| C5 | `zip-archive` resolves to a Hackage version in `cabal configure` output | `clean-slate-ci-run-resolves-all-dependencies` | Log excerpt shows `zip-archive` version resolved (e.g., `zip-archive-0.3.2.2`) |

**Exact test commands** (local equivalent for pre-merge verification):
```bash
# C1 + C5: Clean-slate build
rm -rf ~/.cabal/packages/hackage.haskell.org
nix-shell shell.nix --command 'devenv tasks run ci:build'
# Verify: exit 0, check logs for "zip-archive-0.3" in configure output

# C2: Tests
nix-shell shell.nix --command 'devenv tasks run ci:test'
# Verify: exit 0
```

**FAIL boundaries**:
- FAIL if `Build` step still shows `unknown package: zip-archive` — indicates Task 1's guard is not working
- FAIL if `Build` step shows `Could not resolve dependencies` — same root cause, Task 1's `cabal update` is not running or not completing
- FAIL if `Run tests` step is skipped (because it depends on `ci:build@succeeded`) — indicates Task 1's fix is incomplete
- C4 (OpenSpec validation) FAIL is non-blocking: `continue-on-error: true` means it's informational only; a spec schema failure does not block this task from PASSing if the build itself succeeds

### Affected Modules

None — this task only observes CI output, no code or config changes.

### Prerequisites

- Task 1 is complete and pushed (branched or merged).
- Task 2 passes (`openspec validate --changes --json` succeeds).
- GitHub Actions CI workflow is configured to run on the PR/branch.

### Risks

| Risk | Mitigation |
|------|------------|
| `ci:test` depends on `ci:build@succeeded` — if build is flaky, test step is skipped | Accept: this is expected behavior; the test step correctly only runs after a successful build |
| `Build` passes but `Run tests` fails on an unrelated issue | Report separately — do NOT block this change; this change's scope ends at `ci:build` succeeding |
| CI is transiently flaky (Hackage down during verification) | D2's guard should handle warm-cache runs; for a clean-slate run during Hackage outage, retry the verification |

### Dependency graph

- **Depends on**: Task 1 (guarded `cabal update` must be in place), Task 2 (spec validation must pass).
- **Final verification task** — confirms the full change is correct end-to-end.
