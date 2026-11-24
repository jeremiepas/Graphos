## Tasks

Ordered by dependency. Each task is spec-authoring or verification; no code build is
required because this change is additive to the `product-owner-graphos` spec.

- [x] **1. Author proposal and design**
  - Plan: write `proposal.md` (why/what/capabilities/impact) and `design.md`
    (context/goals/non-goals/decisions/verification) for the status gate.
  - Check criteria: proposal references the AVI-1169 objective and the existing
    `product-owner-graphos` spec; design lists alternatives considered and a spec-only
    verification strategy; neither introduces Haskell/build scope.
  - Done: `proposal.md` quotes the AVI-1169 objective and scopes the change to the
    `product-owner-graphos` spec; `design.md` lists the terminal-state, all-or-nothing
    and durable-recording decisions with alternatives considered, plus an
    `openspec validate --strict` verification strategy. No Haskell, cabal, or
    OpenSpec-core scope.
  - Blocks: none.

- [x] **2. Write the ADDED Requirements + scenarios**
  - Plan: add the task-status-gate requirements and WHEN/THEN scenarios to
    `specs/product-owner-graphos/spec.md` under an `## ADDED Requirements` header,
    matching the existing spec style.
  - Check criteria: every requirement uses `SHALL`/`MUST`; each scenario uses exactly
    four `####` headers and `WHEN`/`THEN`; scenarios cover (a) all blockers resolved →
    backlog, (b) any pending blocker → stays blocked, (c) partial resolution → stays
    blocked, (d) durable recording of the gate outcome, and (e) a cancelled blocker
    counts as resolved.
  - Done: two `SHALL` requirements and five `####` scenarios covering (a)–(e) in
    `specs/product-owner-graphos/spec.md`; terminal-state semantics (`done`,
    `cancelled`) match the blocked ↔ backlog gate in `AGENTS.md`.
  - Blocks: task 1.

- [x] **3. Validate the change**
  - Plan: manually validate the new change against the OpenSpec spec-driven rules (every
    requirement uses `SHALL`/`MUST`; every scenario uses exactly four `####` headers with
    `WHEN`/`THEN`) and confirm no existing requirement was altered; confirm the gate
    logic is consistent with the rest of the `product-owner-graphos` spec.
  - Check criteria: validation passes; diff shows purely additive changes to
    `product-owner-graphos`.
  - Done: `openspec validate AVI-1169-gate-status-task --strict` reports
    `Change 'AVI-1169-gate-status-task' is valid` (openspec 1.8.0), and
    `openspec show --json` lists two deltas, both `product-owner-graphos` / `ADDED` —
    no `MODIFIED` or `REMOVED` section, so no existing requirement is altered.
    The change touches no Haskell source, so `cabal build all` and
    `cabal test graphos-test` are only a sanity check: they pass
    (1877 examples, 0 failures, 4 pending).
  - Blocks: task 2.

- [x] **4. Commit, push, open PR**
  - Plan: commit the change artifacts on branch
    `AVI-1169-gate-status-task`, push to `origin`, and open a PR per the Graphos
    `AGENTS.md` mandatory finish step; record a `pull_request` work product on
    AVI-1169.
  - Check criteria: PR opened against `origin/main`; branch is not silently abandoned.
  - Done: artifacts landed on `main` in commit `0f343c4` via PR #59; this close-out
    commit ticks the tasks with their evidence on branch
    `feature/AVI-1169-gate-status-task` and is opened as PR #284 against `main`.
  - Blocks: task 3.
