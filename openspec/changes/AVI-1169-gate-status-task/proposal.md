## Why

The Product Owner (PO) sequences graphos development with Paperclip child issues and
`blockedByIssueIds`, and the archived `add-product-owner-agent` change documents its
backlog-ownership remit. But the single most frequent triage decision has never been
pinned down: whether a task currently marked `blocked` may safely return to the
`backlog`. Today that judgment is implicit, applied ad hoc, and inconsistent across
heartbeats. The objective for AVI-1169 asks the PO to "check status task put in blocked
if they can be put back in backlog because children or task-blocker are done etc" —
i.e., gate task status on whether its child tasks and task-blockers (`blockedByIssueIds`)
are resolved.

The consequence of leaving this implicit is real: a task can linger in `blocked` long
after its blockers resolve (stale blocking, wasting the developer team's attention), or
be moved to `backlog` while a blocker is still open (premature unblock, dispatching work
that cannot yet run). A documented status gate makes the decision deterministic and
reviewable.

## What Changes

- Add an explicit **status gate** to the `product-owner-graphos` spec: a `blocked` task
  returns to `backlog` only when every child task and every `blockedByIssueIds`
  blocker reports completed; otherwise it stays (or is placed) in `blocked`.
- Require the PO to re-run that gate on every triage pass and record the resulting
  transition — or the explicit reason a task stays blocked — durably (comment or
  document), so the decision is not held only in chat.
- Encode the gate as `SHALL` requirements plus WHEN/THEN scenarios, matching the
  existing `product-owner-graphos` spec style.

## Capabilities

### New Capabilities

- `product-owner-graphos` (extended): task-status gating keyed on child-task and
  task-blocker (`blockedByIssueIds`) resolution.

### Modified Capabilities

- The `product-owner-graphos` spec gains additive requirements; no existing requirement
  is altered.

## Impact

- **Specs**: purely additive to `openspec/changes/AVI-1169-gate-status-task/specs/`
  `product-owner-graphos/spec.md`, delta onto the existing `product-owner-graphos` spec.
- **Code**: none — this documents PO behavior; it does not touch Haskell sources, the
  cabal file, or OpenSpec core.
- **Process**: shapes how the PO agent performs backlog triage on each wake, converting
  an implicit verdict into a repeatable, recorded rule.
