## Tasks

Ordered by dependency. Each task is spec-authoring or verification; no code build is
required because this change is additive to the `product-owner-graphos` spec.

- [ ] **1. Author proposal and design**
  - Plan: write `proposal.md` (why/what/capabilities/impact) and `design.md`
    (context/goals/non-goals/decisions/verification) for the status gate.
  - Check criteria: proposal references the AVI-1169 objective and the existing
    `product-owner-graphos` spec; design lists alternatives considered and a spec-only
    verification strategy; neither introduces Haskell/build scope.
  - Blocks: none.

- [ ] **2. Write the ADDED Requirements + scenarios**
  - Plan: add the task-status-gate requirements and WHEN/THEN scenarios to
    `specs/product-owner-graphos/spec.md` under an `## ADDED Requirements` header,
    matching the existing spec style.
  - Check criteria: every requirement uses `SHALL`/`MUST`; each scenario uses exactly
    four `####` headers and `WHEN`/`THEN`; scenarios cover (a) all blockers resolved →
    backlog, (b) any pending blocker → stays blocked, (c) partial resolution → stays
    blocked, and (d) durable recording of the gate outcome.
  - Blocks: task 1.

- [ ] **3. Validate the change**
  - Plan: manually validate the new change against the OpenSpec spec-driven rules (every
    requirement uses `SHALL`/`MUST`; every scenario uses exactly four `####` headers with
    `WHEN`/`THEN`) and confirm no existing requirement was altered; confirm the gate
    logic is consistent with the rest of the `product-owner-graphos` spec.
  - Check criteria: validation passes; diff shows purely additive changes to
    `product-owner-graphos`.
  - Blocks: task 2.

- [ ] **4. Commit, push, open PR**
  - Plan: commit the change artifacts on branch
    `AVI-1169-gate-status-task`, push to `origin`, and open a PR per the Graphos
    `AGENTS.md` mandatory finish step; record a `pull_request` work product on
    AVI-1169.
  - Check criteria: PR opened against `origin/main`; branch is not silently abandoned.
  - Blocks: task 3.
