## Tasks

Ordered by dependency. Each task is spec-authoring or verification; no code build is
required because this change is additive to the `product-owner-graphos` spec and release policy.

- [ ] **1. Author proposal and design**
  - Plan: write `proposal.md` (why/what/capabilities/impact) and `design.md`
    (context/goals/non-goals/decisions/verification) for the merge-gate status gate.
  - Check criteria: proposal references the AVI-1975 objective and the existing
    `product-owner-graphos` spec; design lists alternatives considered and a
    spec-only verification strategy; neither introduces Haskell/build scope.
  - Blocks: none.

- [ ] **2. Write the ADDED Requirements + scenarios**
  - Plan: add the merge-gate status gate requirements and WHEN/THEN scenarios to
    `specs/product-owner-graphos/spec.md` under an `## ADDED Requirements` header,
    matching the existing spec style used by AVI-1965, AVI-1967, and AVI-1970.
  - Check criteria: every requirement uses `SHALL`/`MUST`; each scenario uses exactly
    four `####` headers and `WHEN`/`THEN`; scenarios cover (a) review lane AVI-1911 done
    → merge-gate lane eligible, (b) review lane AVI-1911 not done → merge-gate lane stays
    blocked, (c) partial/mixed review-lane AVI-1911 state → merge-gate lane stays blocked,
    and (d) durable recording of the gate outcome on the Paperclip issue.
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
    `AVI-1975-merge-gate-pr-200-blockedBy-avi1910`, push to `origin`, and open a PR
    per the Graphos `AGENTS.md` mandatory finish step; record a `pull_request` work
    product on AVI-1975.
  - Check criteria: PR opened against `origin/main`; branch is not silently abandoned.
  - Blocks: task 3.