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

## Task status gating — blocked ↔ backlog

Apply this gate when triaging a task's status:

- A task stays `blocked` while ANY of its first-class blockers (`blockedByIssueIds`) or its direct children are non-terminal. Non-terminal means `todo`, `in_progress`, `in_review`, `backlog`, or `blocked`.
- A task returns to `backlog` (or `todo` when it is ready to start) only once EVERY blocker AND every direct child reaches a terminal state (`done` or `cancelled`).
- A `cancelled` blocker or child counts as resolved. A task whose blockers are all terminal must not remain `blocked` — release it to backlog.
- Keep the gate as `blocked` (never force-release) when any blocker is still active, even if that blocker is itself stuck awaiting a board-operator disposition. Escalate the stuck upstream task separately; do not override the gate on its account.
- Express blockers with first-class `blockedByIssueIds`, never prose. Never move a task off `blocked` on the strength of a comment alone — confirm the blocker's terminal state via the issue object.

## Definition of Done (before `status=done`)

A run is **not** done until all of the following hold, and the verification result is recorded on the issue. Paperclip will not auto-resolve a prose-only `done` claim — a missing work product leaves the issue `in_progress`/`missing disposition` and requires a board decision.

1. **Status PATCH persisted.** The PATCH carrying `status=done` is made with the `X-Paperclip-Run-Id` header; the returned JSON is read back and echoes `status=done`. A comment-only claim is never sufficient.
2. **At least one work product registered for feature work** — one of:
   - `pull_request` with the PR number (preferred), or
   - `branch` work product naming the retained branch and why no PR exists yet, or
   - an uploaded `artifact` / task document.
3. **Evidence in a comment.** The final comment names the commit/branch/PR and the verification command(s) run (`build`/`test`), with the work-product ID or PR link.
4. **No "done via comment only"** — an informal prose `done` with no registered PR/work product/artifact is not a disposition and blocks promotion to `done`.