## Why

OpenSpec specs and changes are the source of truth for feature requirements and task tracking, but there is no CI enforcement that they remain valid. Currently 3 out of 17 specs/changes fail validation (`gitignore-parsing`, `leiden-scalability`, `graphos-product`) — and this was only discovered by manually running `openspec validate`. Without CI validation, spec drift accumulates silently, broken specs go unnoticed, and the contract between proposals and implementation degrades over time.

Adding a minimal, advisory OpenSpec validation step to CI makes spec health visible on every push/PR without blocking merges.

## What Changes

Add a single non-blocking CI step to `.github/workflows/haskell.yml` that runs `openspec validate --changes` after the existing build and test steps. This step:

- Installs OpenSpec CLI via npm
- Validates all active (in-progress) changes
- Outputs results to the GitHub Actions log
- Uses `continue-on-error: true` so it never blocks a build

No changes to the Haskell build, test, or release pipelines. No architecture gates (that's a separate change). No spec compliance checking — just format and structure validation.

## Capabilities

### New Capabilities
- `ci-openspec-validation`: Advisory OpenSpec validation step in GitHub Actions CI that validates active changes on every push/PR, visible in the build log but never blocking

### Modified Capabilities
<!-- No existing capabilities are modified — this is a pure addition to CI -->

## Impact

- **CI**: One new step in `haskell.yml` (~10s added time for npm install + validation)
- **Dependencies**: Adds `openspec` npm package as a CI dependency (not a project dependency)
- **Code**: No Haskell code changes
- **Workflow**: PR authors will see OpenSpec validation results in the Actions log; maintainers can use the signal to prioritize spec fixes

## PDCA Cycle

- **Plan**: OpenSpec validation runs on every push/PR via `openspec validate --changes --json`. Currently 8/9 active changes pass (1 fails with 2 issues). The CI step makes this visible automatically.
- **Do**: Add `npm install -g openspec` and `openspec validate --changes` step to `haskell.yml` with `continue-on-error: true`.
- **Check**: CI log shows validation results. Build status remains green regardless of validation outcome. Currently-failing `graphos-product` change shows as a warning, not a failure.
- **Act**: If validation surface grows (e.g., architecture gates), promote to a separate workflow file. Fix the 3 legacy failing specs when convenient — not blocking.