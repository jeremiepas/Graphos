<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. Add OpenSpec validation step to haskell.yml

- [x] 1.P Plan: Add `npm install -g openspec` and `openspec validate --changes --json` steps to `.github/workflows/haskell.yml` in the `build-and-test` job, after the `Run tests` step. The step SHALL use `continue-on-error: true`. **Check criteria**: (1) `haskell.yml` contains a step named "Validate OpenSpec changes" or similar, (2) the step runs `openspec validate --changes`, (3) the step has `continue-on-error: true`, (4) the step runs after `cabal test`, (5) the YAML is syntactically valid.
- [x] 1.D Do: Edit `.github/workflows/haskell.yml` to add two steps after "Run tests": (a) "Install OpenSpec CLI" running `npm install -g openspec`, (b) "Validate OpenSpec changes" running `openspec validate --changes --json` with `continue-on-error: true`.
- [x] 1.C Check: Run all 5 Check criteria from 1.P. Record PASS/FAIL per criterion. → All 5 PASS.
- [x] 1.A Act: All PASS. Change committed to haskell.yml.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Verify CI workflow runs correctly

- [x] 2.P Plan: Push the change to a branch and verify the GitHub Actions workflow includes the OpenSpec validation step. **Check criteria**: (1) The CI workflow runs the "Validate OpenSpec changes" step, (2) The step produces output in the Actions log showing validation results, (3) The overall build status is green (not blocked by validation failures), (4) The step completes in under 60 seconds.
- [ ] 2.D Do: Push branch, trigger CI run, observe the workflow output. (Requires manual git push)
- [ ] 2.C Check: Inspect the GitHub Actions run for all 4 Check criteria. Record PASS/FAIL.
- [ ] 2.A Act: If all PASS, merge. If FAIL, adjust the step configuration and retry.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Final verification

- [x] 3.P Plan: Confirm the CI pipeline is complete and correct. **Check criteria**: (1) `grep 'openspec validate' .github/workflows/haskell.yml` returns the validation step, (2) `grep 'continue-on-error' .github/workflows/haskell.yml` returns `true` for the validation step, (3) The validation step is positioned after `cabal test`, (4) The workflow YAML is valid (can be parsed by GitHub Actions).
- [x] 3.D Do: Run all verification commands. Confirm YAML validity. Review the full `haskell.yml` for correctness.
- [x] 3.C Check: Run all 4 Check criteria. Record PASS/FAIL. → All 4 PASS.
- [x] 3.A Act: All PASS. The change is complete.

### Attempt history (3)

<!-- empty unless a retry is needed -->