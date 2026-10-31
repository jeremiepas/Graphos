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

## Runtime reliability preflight (watchdog-owned)

The CTO periodic watchdog routine (AVI-1417) is the single owner of the daily agent-health digest. No ad hoc "board action needed" comment may substitute for a linked approval/interaction.

1. List every agent whose `status == "error"` (and treat agents `running` with a non-empty `errorReason` as degraded). For each entry include the agent id, name, `errorReason`, and a provider/usage-limit indicator when the adapter depends on a hosted provider (for example Ollama weekly usage limit for a given user, or connection refused).
2. Emit exactly one board-approval path, attached to the watchdog issue:
   - an `ask_user_questions` or `request_board_approval` interaction, OR
   - one tracked follow-up issue assigned to the owner who must act,
   naming the exact agent ids and the single action a human must take. Issue or comment prose alone is not a disposition.
3. Secrets-binding verification: for agents that require a secret (BW_PASSWORD, S3 keys), check `GET /api/agents/me/secrets` in that agent's own context (or document the check in the agent's AGENTS) and fail the digest when the binding is missing by creating the approval/interaction above. A missing binding is never silently retried.
4. Provider/model/account rotations stay board-gated: an agent may not PATCH its own model or invoke another agent heartbeat; the watchdog routes the exact rotation to the responsible head or board instead of attempting self-remediation.
5. Resource ceilings (memory/CPU) that caused an OOM or equivalent failure are recorded with before/after values in the owning project's config and verified by a later successful operation. The CTO routes the change; the project head owns implementation. Any RustFS memory raise must be recorded in the linked issue with the previous and new limits and a later successful image push/run.
6. Verification: re-run the watchdog sweep and confirm every previously-error agent is `ready`/`idle`, or a linked approval card exists carrying their ids.
