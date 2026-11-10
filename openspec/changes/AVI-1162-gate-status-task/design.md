## Context

The `add-product-owner-agent` change hired the Product Owner (PO) and documented its
backlog-ownership remit, including dispatching developer-ready child issues with
`blockedByIssueIds` for dependency ordering. Since then, the PO has performed backlog
triage without a single authoritative rule for the most frequent decision: whether a
blocked task can return to the backlog. The objective for AVI-1162 asks the PO to
"check status task put in blocked if they can be put back in backlog because children
or task-blocker are done etc" — i.e., gate task status on whether its children and
task-blockers are resolved.

This change makes that decision explicit and deterministic by adding requirements to
the `product-owner-graphos` spec. It is a behavioral/spec-level change: it touches no
Haskell source, cabal file, or OpenSpec core workflow.

## Goals / Non-Goals

**Goals:**
- Gate each task's status on child-task and `blockedByIssueIds` resolution: resolved
  → back to `backlog`; any pending → stays `blocked`.
- Require the PO to re-run the gate on every triage pass and record the outcome
  durably (comment or document), not only in chat.
- Keep the change purely additive to the existing `product-owner-graphos` spec.

**Non-Goals:**
- Changing Paperclip's own status-machine semantics; this documents PO behavior, it
  does not modify how the board stores statuses.
- Altering existing requirements in the `product-owner-graphos` spec or any other
  live spec.
- Writing or reviewing Haskell code, or changing the build/test pipeline.
- Deciding which statuses exist on the board (that is a company/board decision).

## Decisions

- **Encode the gate as OpenSpec `SHALL` requirements + WHEN/THEN scenarios** in the
  `product-owner-graphos` spec (the canonical home for PO behavior), rather than
  scattering prose across chat comments.
  - *Alternative considered:* a free-text rule in the PO `AGENTS.md` bundle — rejected
    because it is not testable, not versioned with the change, and not reviewable via
    `openspec check`.
  - *Alternative considered:* a Paperclip board automation rule — rejected because the
    board's status machine is out of the PO's authority; the PO only triages.
- **Resolve-on-completion semantics** (a blocker counts as resolved only when the
  blocking issue reports completed, not merely "in progress" or "review").
  - *Alternative considered:* treat in-review blockers as resolved — rejected, it can
    prematurely unblock work whose blocker is still under review.
- **Record every gate outcome durably** (comment or document) so partial or no-change
  decisions survive across heartbeats.
  - *Alternative considered:* only act on changes — rejected, silent no-ops leave the
    rationale unreconstructable.

## Verification

- `openspec check` (or equivalent validation) passes for the new change: every
  requirement uses `SHALL`/`MUST` and every scenario uses exactly four `####` headers
  with `WHEN`/`THEN`.
- Manual read-through confirms the gate logic matches the AVI-1162 objective and is
  consistent with the existing `product-owner-graphos` requirements (no contradictions,
  no modified requirements).
- No Haskell build/test is required; the change is spec-only.
