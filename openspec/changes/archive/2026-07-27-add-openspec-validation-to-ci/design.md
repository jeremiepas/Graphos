## Context

The project has 9 OpenSpec changes and 8 specs, but no CI enforcement that they remain valid. Currently 3 out of 17 items fail validation (`gitignore-parsing`, `leiden-scalability` — missing Purpose/Requirements sections; `graphos-product` — 2 requirements missing SHALL/MUST). These failures were only discovered by manually running `openspec validate --all`. The existing CI pipeline (`haskell.yml`) runs `cabal build` and `cabal test` but has no spec health visibility.

OpenSpec CLI (v1.4.1) is already installed locally via npm. The `--changes` flag validates only active/in-progress changes, making it suitable for CI without blocking on legacy specs.

## Goals / Non-Goals

**Goals:**
- Make OpenSpec validation results visible on every push/PR in GitHub Actions
- Validate active changes (`--changes`) — not legacy specs that predate the format
- Keep the CI step advisory (non-blocking) so spec drift doesn't block merges
- Keep added CI time under 15 seconds

**Non-Goals:**
- No blocking of PRs on validation failures (advisory only)
- No fixing of legacy specs (gitignore-parsing, leiden-scalability, graphos-product)
- No architecture gate checks (separate change: refactor-architecture-ports-and-split-god-modules Task 12)
- No spec-to-code compliance checking
- No `--all` validation (would require fixing legacy specs first)
- No separate workflow file (keep it in existing `haskell.yml`)

## Decisions

### D1: Validate active changes only (`--changes`), not all specs (`--all`)

| Aspect | Choice |
|--------|--------|
| **Decision** | Use `openspec validate --changes` in CI |
| **Rationale** | `--changes` validates only in-progress changes, avoiding failures from 3 legacy specs that predate the format. This matches the "apply spec" philosophy: specs being worked on should be valid. |
| **Alternatives** | (A) `--all --strict` — would block CI until 3 legacy specs are fixed first. (B) `--all` without `--strict` — same issue, 3 failures noise the signal. (C) No validation at all — current state, spec drift accumulates silently. |

### D2: `continue-on-error: true` — advisory, not blocking

| Aspect | Choice |
|--------|--------|
| **Decision** | The CI step uses `continue-on-error: true` so validation failures never block the build |
| **Rationale** | Spec validation is a health signal, not a gate. Making it blocking would require fixing all legacy specs first and could block urgent hotfixes. Advisory mode gives visibility without friction. |
| **Alternatives** | (A) Blocking gate (`continue-on-error: false`) — too aggressive for initial rollout. (B) Separate GitHub check status — requires more workflow setup, not minimal. |

### D3: Install OpenSpec via npm in CI step

| Aspect | Choice |
|--------|--------|
| **Decision** | Add `npm install -g openspec` as a step in the existing `haskell.yml` workflow |
| **Rationale** | Minimal change — no new workflow file, no nix integration, no caching complexity. OpenSpec is an npm package; a single `npm install -g` is the simplest path. |
| **Alternatives** | (A) Add openspec to `shell.nix` — heavier, couples CI tool to nix. (B) Separate workflow file — more YAML, more maintenance, not minimal. (C) Pin exact version — good practice but not required for initial rollout; can add later. |

### D4: Place validation step after build and test

| Aspect | Choice |
|--------|--------|
| **Decision** | The validation step runs after `cabal build` and `cabal test`, not before |
| **Rationale** | Build and test are the real gates. OpenSpec validation is supplementary. If build fails, spec validation is irrelevant. Running after means it only runs on green builds. |
| **Alternatives** | (A) Before build — wastes time if build would fail anyway. (B) Separate parallel job — more setup for minimal gain on a ~10s step. |

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| npm install adds ~5-10s to CI | Acceptable for advisory step; can pin version and cache later if needed |
| `continue-on-error: true` means failures are easy to ignore | Start advisory; promote to blocking after team habit forms and legacy specs are fixed |
| `--changes` doesn't validate legacy specs | Intentional — legacy spec cleanup is a separate task, not blocking CI |
| OpenSpec CLI version drift between local and CI | Pin version in CI step once initial rollout stabilizes (e.g., `npm install -g openspec@1.4.1`) |

## Verification Strategy (Check)

| Gate | Verification | Command |
|------|-------------|---------|
| CI step exists | `haskell.yml` contains `openspec validate` step | `grep 'openspec validate' .github/workflows/haskell.yml` |
| CI is advisory | Step has `continue-on-error: true` | `grep 'continue-on-error' .github/workflows/haskell.yml` |
| Validation runs on push/PR | Step is in `build-and-test` job, not gated behind manual trigger | Visual inspection of workflow YAML |
| Validation output visible | `--json` output is teed to log | CI run log shows validation results |
| Build remains green even if specs fail | Push a broken spec change; CI build status is green | GitHub Actions build status |

## Iteration & Rollback (Act)

**If Check fails:**
- CI step missing → add the step to `haskell.yml`
- Step blocks builds → verify `continue-on-error: true` is set
- OpenSpec install fails → check npm availability on `ubuntu-latest` runner

**Rollback:** Single step removal from `haskell.yml`. No other changes to revert.

**Standardization for next cycle:**
- Pin OpenSpec version once team is comfortable (`openspec@1.4.1`)
- Add `--strict` flag once all legacy specs are fixed
- Consider promoting to blocking gate once spec hygiene is established
- Add architecture invariant checks (separate change from the refactoring)