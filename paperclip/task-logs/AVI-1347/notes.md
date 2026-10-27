# AVI-1347 — correct AVI-984-group3-convert-wip merge (restore main CI) + verify green

- **Status:** `in_progress` (high) — this is the current focus.
- **Current focus:** Verify the AVI-984 group-3 conversion merge landed cleanly on
  `main`, restore main's CI, and confirm a green build + test run.
- **Blockers:** none on this task; blocked upstream items
  ([AVI-1349](/AVI/issues/AVI-1349), [AVI-1350](/AVI/issues/AVI-1350),
  [AVI-963](/AVI/issues/AVI-963)) are separate Route-1323 work.

## Context (from inbox)

- Linked parent work: AVI-984 group-3 conversion, AVI-1333.
- Priority `high`; currently the highest-priority `in_progress` task with an
  active run.

## Next steps

- [x] Confirm `main` CI is restored and green after the merge. → done: head `debb9e9`
      byte-identical to `main` @ `71f9a95`; `cabal build all` Up to date;
      `cabal test` 27 examples / 0 failures (`graphos-test-focus` PASS).
- [x] Record finish outcome (push + PR / keep / close) per branch-and-PR hygiene. →
      EXPLICIT KEEP recorded: primary `branch` work product `926beb2e-e108-43ce-8d04-16860df77e76`.
- [ ] **Issue close (`done` + report comment) is BLOCKED by the Heartbeat run-context
      guard** — every run on this issue is sandboxed to `inspect_run`; task-binding is
      board-gated. See `run-2026-10-23-finalize.md` for the exact finish steps. Needs a
      board operator to launch a task-bound run (or the issue's `originRunId`/plan action).
