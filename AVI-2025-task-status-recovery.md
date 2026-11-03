# AVI-2025 — Check Task Status Recovery Need Who to Fix

deliberate_wait_without_target → set blocked disposition + recorded resolution note → hand back to cto

**Issue**: AVI-2025 — check task status recovery need who to fix ?

**Assigned**: CTO agent (fc005152-e2a9-4ddb-8fbc-ed6325deb114)

**Status**: blocked

**Disposition**: blocked — board-owned recovery action active (AVI-1221: bb6fe270, kind=missing_disposition, cause=successful_run_missing_state, owner=board). Run-context boundary prevents ordinary agent PATCH/write operations.

**Resolution Note**: Recovery from deliberate_wait_without_target: issue finished without recorded disposition, activating board-owned recovery action on AVI-1221. Cause: run finished without setting status disposition. Action: set blocked disposition, documented board escalation path via AVI-1666. Hand back to cto for board/operator resolution (admin/recovery path required). Next: board/operators resolve bb6fe270 via admin/recovery; head-of-dev provision task-scoped run for AVI-1221; merge-gatekeeper resolve AVI-1845 self-approval policy.

## Investigation Summary

Checked recovery status across the avionix portfolio (Graphos, homelab, nixos). Key findings:

### AVI-1845 — Recovery Completed
- Recovery from `deliberate_wait_without_target` completed successfully
- Branch rebased onto origin/main, PR #151 updated
- CI all green; 4 APPROVE verdicts posted (self-approval blocked by GitHub policy)
- Warnings-as-errors fixed (AVI-1860)
- **Next**: merge-gatekeeper to merge once CI green + review approved (self-approval policy resolution)

### AVI-1221 — Active Board Recovery Action
- **Recovery action `bb6fe270`**: kind=missing_disposition, cause=successful_run_missing_state, owner=board
- **Reason**: This run finished without setting a disposition, activating the board-owned recovery action
- **Blocker**: Control-plane writes (PATCH status=done, POST comments) return HTTP 403 due to run-context boundary
  - Even after successful checkout, writes still 403 because the run is unassigned
  - The run-id header must match the signed JWT and cannot be swapped for the issue's originRunId
- **Escalation**: AVI-1666 created and assigned to head-of-dev (status in_progress)
  - Ask: "provision a task-scoped run for AVI-1221 OR perform the done-write + record the branch work product directly"
- **Cannot be resolved by ordinary agent**: run-context boundary prevents PATCH/write operations

### AVI-1910 — Board Recovery, Disposition Exhausted
- Board recovery owner; 2/2 disposition repair attempts exhausted
- PR #201 review deferred pending board decision

## Determination: Who to Fix

The AVI-1221 recovery action `bb6fe270` has `owner=board`. The run-context boundary (unassigned heartbeat run) prevents ordinary agents from completing the required PATCH/write operations. Therefore:

- **Board/operators** must resolve the recovery via the admin/recovery path
- **CTO** should escalate to the board and ensure AVI-1666 is fulfilled (provision task-scoped run or directly perform the done-write + record branch work product)
- **head-of-dev** (AVI-1666 assignee) should provision a task-scoped run or perform the closure actions

## Recommended Next Actions

1. **CTO**: Escalate AVI-1221 recovery to the board via admin/recovery path; ensure AVI-1666 assignee (head-of-dev) is positioned to provision a task-scoped run
2. **Board/operators**: Resolve recovery action `bb6fe270` on AVI-1221 via the admin/recovery path
3. **head-of-dev** (AVI-1666 assignee): Provision a task-scoped run for AVI-1221 OR perform the done-write + record the branch work product directly
4. **merge-gatekeeper**: Resolve AVI-1845 self-approval policy constraint (4 APPROVE verdicts posted, GitHub blocks auto-approval since PR authored by same user)

## Work Products Registered

- this analysis file: Graphos/AVI-2025-task-status-recovery.md
