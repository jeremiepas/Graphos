# Branch & PR hygiene (mandatory finish step)

Every run that creates or commits to a git branch in this repository must end
with exactly one of three recorded outcomes on its Paperclip issue:

1. **Push + open PR** (default): push the branch to `origin`, open a pull
   request, and record a `pull_request` work product. Never end a run with
   commits that exist only on a local branch.
2. **Explicit keep**: record a `branch` work product naming the branch and why
   the PR is deferred (WIP, planned follow-up).
3. **Explicit close**: delete the local branch only when it is fully merged into
   `origin/main` or superseded, and record the decision in an issue comment.

## Hard rules

- Never silently abandon a branch. A run that committed to a branch and records
  none of the three outcomes is incomplete.
- No force-push, ever.
- Never delete an unmerged branch without a recorded human verdict on the issue.
- Branch naming: `<type>/AVI-<issue-number>-<slug>` (e.g.
  `feature/AVI-974-review-cosmetics`).
- Merging is the exclusive authority of the merge-gatekeeper. Opening a PR is
  the default ending for coding work.

## Task status gating — blocked ↔ backlog

- A task stays `blocked` while ANY of its first-class blockers
  (`blockedByIssueIds`) or its direct children are non-terminal.
- It returns to `backlog` (or `todo` when ready) only once every blocker AND
  every direct child reaches a terminal state (`done` or `cancelled`).
- A `cancelled` blocker/child counts as resolved.
- Keep `blocked` (never force-release) while any blocker is still active;
  escalate the stuck upstream task separately — do not override the gate.
- Express blockers with first-class `blockedByIssueIds`, never prose.
