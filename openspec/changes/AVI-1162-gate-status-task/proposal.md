## Why

The Product Owner (PO) already sequences work with Paperclip child issues and
`blockedByIssueIds`, and the archived `add-product-owner-agent` change documents its
backlog-ownership remit. But nothing pins down the single most common triage decision:
whether a blocked task can safely return to the backlog. Today that judgment is
implicit, applied ad hoc, and inconsistent across heartbeats. The result is that a
task can linger in `blocked` long after its blockers resolve (stale blocking), or be
moved to `backlog` while a blocker is still open (premature unblock). Both waste the
developer team's attention. A documented status gate makes the decision deterministic
and reviewable.

## What Changes

- Add an explicit **status gate** to the `product-owner-graphos` spec: a blocked task
  returns to `backlog` only when every child task and every `blockedByIssueIds`
  blocker is completed; otherwise it stays `blocked`.
- Require the PO to re-run that gate on every triage pass and record the outcome
  durably (comment or document), so the decision is not held only in chat.
- Encode the gate as OpenSpec `SHALL` requirements plus WHEN/THEN scenarios, matching
  the existing `product-owner-graphos` spec style.

## Capabilities

### New Capabilities
- `product-owner-graphos` (extended): task-status gating keyed on child-task and
  task-blocker resolution.

### Modified Capabilities
- The `product-owner-graphos` spec gains additive requirements; no existing requirement
  is altered.

## Impact

- **Specs**: purely additive to `openspec/specs/product-owner-graphos/spec.md`.
- **Code**: none — this documents PO behavior; it does not touch Haskell sources, the
  cabal file, or OpenSpec core.
- **Process**: shapes how the PO agent performs backlog triage on each wake.
