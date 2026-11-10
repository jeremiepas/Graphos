## Tasks

Ordered by dependency. Each task is spec-authoring or verification; no code build is required because this change is additive to the `product-owner-graphos` spec and release policy.

- [x] **1. Author proposal and design**
  - Plan: write `proposal.md` (why/what/capabilities/impact) and `design.md` (context/goals/non-goals/decisions/verification) for the merge-gate status gate with PROBE.
  - Check criteria: proposal references the AVI-1893 objective and the existing `product-owner-graphos` spec / release policy; design lists alternatives considered and a spec-only verification strategy; neither introduces Haskell/build scope.
  - Blocks: none.

- [x] **2. Write the ADDED Requirements + scenarios**
  - Plan: add the merge-gate-status-gate PROBE requirements and WHEN/THEN scenarios to
    `specs/product-owner-graphos/spec.md` under an `## ADDED Requirements` header,
    matching the existing spec style.
  - Check criteria: every requirement uses `SHALL`/`MUST`; each scenario uses exactly four
    `####` headers and `WHEN`/`THEN`; scenarios cover (a) PROBE done → review lane may be marked done,
    (b) PROBE not done → review lane stays in current state, (c) partial/mixed state → review lane
    stays blocked, and (d) durable recording of the gate outcome.
  - Blocks: task 1.

- [x] **3. Validate the change**
  - Plan: run `openspec validate --changes` on the new change and confirm
    no existing requirement was altered; confirm the gate logic is consistent with the rest
    of the `product-owner-graphos` spec and release policy.
  - Check criteria: validation passes; diff shows purely additive changes to
    `product-owner-graphos` and release policy docs.
  - Blocks: task 2.

- [x] **4. Commit, push, open PR**
  - Plan: commit the change artifacts on branch
    `feature/AVI-1893-probe-review-lane`, push to `origin`, and open a PR per the Graphos
    `AGENTS.md` mandatory finish step; record a `pull_request` work product on AVI-1893.
  - Check criteria: PR opened against `origin/main`; branch is not silently abandoned.
  - Blocks: task 3.