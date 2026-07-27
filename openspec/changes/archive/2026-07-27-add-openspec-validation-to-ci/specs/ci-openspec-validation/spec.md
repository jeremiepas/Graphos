## ADDED Requirements

### Requirement: CI validates active OpenSpec changes on every push/PR

The `haskell.yml` GitHub Actions workflow SHALL include a step after `cabal test` that installs OpenSpec CLI and runs `openspec validate --changes`. This step SHALL use `continue-on-error: true` so that validation failures never block the build.

- **Plan**: Make spec health visible on every push/PR without blocking merges. Active changes should pass validation; failures are advisory warnings.
- **Do**: Add `npm install -g openspec` and `openspec validate --changes --json` as steps in `haskell.yml` after the test step.
- **Check**: The scenarios below verify the step exists, is advisory, and produces output.
- **Act**: If spec failures become routine, promote to blocking gate. Fix legacy specs separately.

#### Scenario: OpenSpec validation step exists in CI
- **WHEN** inspecting `.github/workflows/haskell.yml`
- **THEN** it contains a step that runs `openspec validate --changes`

#### Scenario: Validation step is advisory (non-blocking)
- **WHEN** the OpenSpec validation step fails (e.g., a spec has invalid format)
- **THEN** the overall CI build status remains green (success)

#### Scenario: Validation runs after build and test
- **WHEN** `cabal build` or `cabal test` fails
- **THEN** the OpenSpec validation step does not run (it is after build+test in the job)

#### Scenario: Validation output is visible in CI log
- **WHEN** the OpenSpec validation step runs
- **THEN** the validation results (pass/fail per change) are written to the GitHub Actions log

#### Scenario: Only active changes are validated
- **WHEN** `openspec validate --changes` runs
- **THEN** it validates only changes with `status: "in-progress"` (not completed changes or standalone legacy specs)