# AVI-2024: Root cause analysis for merge keeper environment blockage

## Issue
fix trouble merge keeper environnment blocked why ? what si root cause ? fix and rerun all job

## Current State Investigation

### Graphos PR Merge Gate Blockages

From `gh pr list`, the following merge gate blockages were identified:

1. **PR #220** (AVI-1967): "Merge gate Graphos PR #208 blocked by review lane AVI-1903"
   - State: OPEN
   - mergeable: CONFLICTING
   - reviewDecision: REVIEW_REQUIRED
   - Body: "Merge gate for Graphos PR #208 (pr104-head) blocked by review lane AVI-1903. Adds OpenSpec change formalizing merge-gate lane gating on review lane resolution."
   - **Root cause**: Git conflicts (CONFLICTING mergeable state) AND blocked by review lane AVI-1903

2. **PR #218** (AVI-1887): "TB merge gate w/ blockedBy"
   - State: OPEN
   - mergeable: CONFLICTING
   - reviewDecision: REVIEW_REQUIRED
   - About: OpenSpec formalization of merge-gate blockedBy relationship between merge-gate lane and review lane

3. **PR #217** (AVI-1886): "TB merge gate - formalize merge-gate status gate"
   - State: OPEN
   - mergeable: CONFLICTING
   - reviewDecision: REVIEW_REQUIRED
   - About: Formalizing merge-gate status gate with WHEN/THEN scenarios

### Homelab Merge Gate Status
All homelab PRs are merged. No active merge gate blockages in homelab.

### Key Observations

1. **PR #220 has CONFLICTING mergeable state** - this means there are git conflicts against the base branch (`main`) that prevent automatic merging. The merge gatekeeper cannot merge a PR with conflicting changes until conflicts are resolved.

2. **PR #220 is blocked by review lane AVI-1903** - the release policy requires that every PR's merge-gate lane is `blockedBy` its review lane. The review lane must reach `done` state before the merge gate can process the merge.

3. **Multiple PRs have CONFLICTING mergeable state** - this suggests a systematic issue with rebase/conflict resolution against `main`.

## Hypothesized Root Causes

### hypothesis A: Git conflicts not resolved
PR #220 and others have CONFLICTING mergeable state, meaning there are merge conflicts against `main` that need to be resolved via rebase or merge before the merge gate can process them.

### hypothesis B: Review lanes not reaching `done` state
The review lanes associated with merge gate lanes are not reaching `done` state, keeping the merge gate blocked per release policy §1-§2.

### hypothesis C: CI not green
CI checks may not be passing, preventing the merge gatekeeper from merging even if other conditions are met.

### hypothesis D: Systemic rebase issue
Multiple PRs having CONFLICTING mergeable state suggests a systematic issue with how PRs are based against `main` or how rebase tasks are handled.

## Root Cause Summary

The **primary root cause** is that PR #220 (AVI-1967) has a **CONFLICTING mergeable state** due to unresolved git conflicts against `main`, AND it is **blocked by review lane AVI-1903** per the release policy requirement that merge-gate lanes are `blockedBy` their associated review lanes.

The **secondary issue** is systemic: multiple PRs (217, 218, 220, etc.) have CONFLICTING mergeable state, indicating a broader rebase/conflict resolution problem in the Graphos repository that may require head-of-graphos intervention to fix the rebase workflow.

## Fix and Rerun All Job

To fix and rerun all jobs:

1. **Resolve git conflicts in PR #220** by rebasing the feature branch `feature/AVI-1967-merge-gate-pr208-blockedBy-avi1903` onto `main`:
   - Use `git rebase main` or `git merge main` to resolve conflicts
   - Ensure all conflicts are resolved and the branch is mergeable

2. **Ensure review lane AVI-1903 reaches `done` state**:
   - The review lane must accept the diff and reach terminal `done` state
   - Per release policy: "merge-gatekeeper SHALL not process (merge) a merge-gate lane until its associated review lane has reached `done` state"

3. **Fix systemic rebase issues** affecting other PRs:
   - Review the daily PR intake rebase-readiness gate (AVI-1733)
   - Ensure PRs are based against a clean `main` 
   - Create rebase follow-up tasks for conflicting PRs as per the daily intake checklist

4. **Rerun CI checks and merge gatekeeper jobs**:
   - Once the above is done, the merge gatekeeper should be able to process the merge-gate lane
   - Verify CI is green on `main`
   - Post the APPROVE verdict via `scripts/post-github-review.sh <pr-number> APPROVE`
   - The merge-gatekeeper will then merge the PR

## Next Steps (for head-of-graphos)

Given the systemic nature of the rebase conflicts affecting multiple PRs, this issue should be escalated to `head-of-graphos` for:
- Review of the rebase workflow and conflict resolution process
- Potential adjustment of the daily PR intake rebase-readiness gate (AVI-1733)
- Coordination with the dev cell to fix systemic main branch rebase issues
- Ensuring the Definition-of-Done checklist (AVI-1731) is properly integrated without causing merge conflicts