# paperclip — company operations home

This directory is the operational home for this Paperclip company's work inside
the `Graphos` repository. It keeps company-side tooling, conventions, and
per-issue working notes out of the Haskell source tree.

## Layout

```
paperclip/
  README.md          # this file — conventions and how to use the tooling
  status.mjs         # `status`: summarize the agent's current assigned tasks
  .gitignore         # scratch / local artifacts that should not be committed
  agents/            # per-agent role notes / operating instructions
  projects/          # project chart — one folder per project (projects/goals)
  task-logs/         # per-issue working notes (one folder per {identifier})
  scripts/           # other small helpers (not part of the Haskell build)
  docs/              # runbooks and conventions
  scratch/           # consolidated ephemeral working notes (moved from .paperclip-scratch/)
```

## Company

- **Company ID:** `77bac7d6-c694-46d7-9302-b28bec8b3a7d`
- **API:** `https://paperclip.avionix.space`
- **Agents:** `devhaskell` (engineer — Haskell for Graphos), plus board/manager agents.

## Working model (dev-cell PDCA loop)

`devhaskell` is the first step of a dev-cell PDCA loop:

1. **dev (this agent)** — implements Haskell for Graphos in a GitFlow worktree,
   pushes the feature branch, opens the PR, requests review. Implements, does
   not validate or merge.
2. **review / validate** — separate agents (reviewer, merge-gatekeeper) verify
   the build + tests and merge.

Rules that apply here (see the repo `AGENTS.md`):

- Never approve or merge your own PR.
- Never work on `main` / `develop` — branch as `<type>/AVI-<n>-<slug>`.
- Every run that commits to a branch ends with a recorded outcome: push + open
  PR (default), explicit keep (branch work product), or explicit close.
- No force-push, ever. Merging is the merge-gatekeeper's authority.

## `status` command

Prints a summary of the agent's current assigned tasks, grouped by status, and
highlights the current focus.

```sh
# inside a Paperclip run (env vars are injected automatically)
node paperclip/status.mjs

# machine-readable
node paperclip/status.mjs --json
```

It reads `PAPERCLIP_API_URL`, `PAPERCLIP_API_KEY`, `PAPERCLIP_AGENT_ID`,
`PAPERCLIP_COMPANY_ID` from the environment (available inside a heartbeat run).
Pass `--api-url` / `--api-key` / `--agent-id` / `--company-id` to override, or
run it from any shell with those vars set.

## Notes

- Live working notes for an issue go in `task-logs/{identifier}/`.
- Ephemeral scratch goes in `scratch/` (gitignored) — do not commit it.
- Keep Haskell code in `src/`, `tests/`, and OpenSpec under `openspec/`; this
  directory is intentionally excluded from the Haskell build.
