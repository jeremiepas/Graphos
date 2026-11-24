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

## mnemo memory graph — the `graphos` space

Mnemo (the harness at `/home/jeremie/Documents/perso/poc-graphos/mnemo`) keeps a memory graph for this repository in Neo4j (`http://localhost:7477`), in the space **`graphos`** — its own space, never `homelab`.

`7477` is **one** Neo4j instance and therefore one console (`http://localhost:7477`) for every space: `homelab`, `graphos`, `console`, `eval` and the rest live side by side in it, told apart by the `space` property (`MATCH (n) WHERE n.space = 'graphos' RETURN n LIMIT 100`). Do not start a Neo4j or a console per space. The `mnemo web` UI is a different thing: it renders exactly one space per process and has no space switcher, so a second view is a second process, not a second database.

- **Config file:** `~/.config/mnemo/graphos.yaml`, selected with `MNEMO_CONFIG=$HOME/.config/mnemo/graphos.yaml`. The default `~/.config/mnemo/config.yaml` is the homelab space (its delivery board, its runs): never edit it for Graphos.
- **Wrapper:** `mnemo-graphos <command>` exports `MNEMO_CONFIG` and runs the pinned build, so `mnemo-graphos status`, `mnemo-graphos anchors check` and `mnemo-graphos web` all act on the `graphos` space. The Graphos console defaults to `127.0.0.1:7481` and the homelab one to `7480` — only because two `mnemo web` processes need two ports; the graph itself is always the single `7477`.
- **Code graph:** this repository is pushed as project `Graphos` into the perso Neo4j (`http://localhost:7474`, shared with the other perso projects). Rebuild it with `mnemo-graphos-project Graphos /home/jeremie/Documents/perso/Graphos`; `poc-graphos` is pushed the same way. graphos' own `--neo4j-push` does not write the `project` property mnemo reads — use the wrapper.
- **Ingest:** `mnemo-ingest-graphos` reads `.opencode/skills/*/SKILL.md`, `.opencode/commands/*.md` and `AGENTS.md` into the space. Its root is the symlink mirror `~/.local/share/mnemo/ingest/graphos`: ingesting the checkout directly would see the duplicate skills of `.worktrees/` and refuse. Run it after editing a skill, a command or this file; a second run is a no-op.
- **Anchors:** `mnemo-graphos anchors add --project Graphos <repo-relative paths>` links memory to code. `anchors check` re-resolves a project in one query capped at 2,000 rows, so keep the anchor set small — 36 anchors resolve cleanly, the whole `src/` tree is reported falsely stale. Verify with `mnemo-graphos anchors check` and expect `stale anchors: 0`.
- **No delivery here:** the `graphos` config has no `delivery` section on purpose. The delivery board (canvas, catalogue, workspaces) is the solario/i2p work in space `homelab`; do not run `mnemo delivery` against `graphos`.
- **Credentials** stay in `~/.config/mnemo/env` (mode 600); no secret goes in either YAML, and no config is ever copied over another.
