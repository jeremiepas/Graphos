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

## Trunk-based integration (Option A — accepted plan)

- `main` is the single integration + production branch. The `develop` GitFlow branch is retired; do not create or target `develop` for new work.
- Per issue AVI-XXX: `git worktree add .worktrees/AVI-XXX -b feature/AVI-XXX main` (worktrees live in the Graphos repo); PRs target `main`.
- Existing worktrees under `.worktrees/` and `.paperclip/worktrees/` belong to other issues — do not touch them.
- The board may override; revisit GitFlow only if release isolation becomes the constraint.

## GitHub approvals — materialize verdicts

- An approving review verdict is not complete until it is posted to GitHub as an approving review (REST API: `POST /repos/{owner}/{repo}/pulls/{n}/reviews` with `event=APPROVE`). Prose-only approval in a Paperclip comment is never sufficient.
- `haskell-reviewer` posts the code-review approval; `merge-gatekeeper` relies on the GitHub approval check to satisfy required-approvals branch protection.
- `REQUEST_CHANGES` / `COMMENT` reviews are posted the same way for red-CI or remediation verdicts.
- Tooling: `scripts/post-github-review.sh <pr-number> <APPROVE|REQUEST_CHANGES|COMMENT> [body]` wraps the REST call; haskell-reviewer MUST call it for every verdict and never rely on `gh pr comment` alone.

## Merge SLA (gatekeeper-exclusive)

- Green + approved PRs are merged by `merge-gatekeeper` within 24 h, oldest-first.
- After each merge, re-check remaining open PRs and file rebase tasks for any new conflicts.
- Only `merge-gatekeeper` merges; no direct pushes to `main`.

## Zombie-GC rule

- No issue may remain `done` while an associated PR is open. On merge/close of the PR, the owning issue moves to a terminal state (`done`/`cancelled`) within 24 h.
- Blocked issues whose associated PR is merged or closed must be released to `backlog`/`todo` (or terminal) within 24 h per the blocked↔backlog gate above.
- The weekly `branch-guardian` audit reports the zombie count; targets: zombie issues = 0, done-issues-with-open-PR = 0. A non-zero count is an escalation trigger.

## Weekly fluidity metrics (branch-guardian audit)

Targets published weekly: open PRs ≤ 5; median PR age open→merge < 48 h; merged < 24 h after green+approved = 100%; conflicting PRs > 24 h = 0; red-CI PRs without a remediation task = 0; zombie issues = 0; done-issues with open PR = 0.