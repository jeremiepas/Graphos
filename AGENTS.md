# Graphos — agent working rules

This file is loaded into every agent run in this repository. Follow it alongside your Paperclip instructions.

## Branch/PR hygiene — mandatory finish step

Every run that creates or commits to a git branch in this repository must end with exactly one of three recorded outcomes on its Paperclip issue:

1. **Push + open PR** (default): push the branch to `origin`, open a pull request, and record a `pull_request` work product on the issue. Never end a run with commits that exist only on a local branch.
2. **Explicit keep**: record a `branch` work product naming the branch and stating why the PR is deferred (work in progress, planned follow-up).
3. **Explicit close**: delete the local branch only when it is fully merged into `origin/main` or superseded, and record the decision in an issue comment.

### Hard rules

- Never silently abandon a branch. A run that committed to a branch and records none of the three outcomes above is incomplete.
- No force-push, ever.
- Never delete an unmerged branch without a recorded human verdict on the issue.
- Branch naming: `<type>/AVI-<issue-number>-<slug>` (for example `feature/AVI-974-review-cosmetics`).
- Merging PRs is the exclusive authority of the merge-gatekeeper; opening a PR is the default ending for coding work.