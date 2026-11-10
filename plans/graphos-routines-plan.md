# Plan — Graphos Paperclip Routines (AVI-1578)

## Context
Request: create Paperclip routines for Graphos covering (1) reviewing open PRs,
(2) merging PRs that are reviewed and mergable, and (3) a "realise" policy plus
a routine to create realise.

**This plan is the only deliverable.** No routine is created until the board
confirms. Once confirmed, implementation is limited to creating the one missing
routine and wiring its schedule trigger.

Workspace copy of this plan (board artifact excluded, per directive):
`plans/graphos-routines-plan.md` on branch `AVI-1578-routines`.

## Findings: two of three requests already satisfied by live routines
Both were created by the CTO agent and are currently live and `active`:

- **PR review intake** — routine `37a8dc3d-9045-4b71-9abc-5a7f46dfc47a`,
  "Daily Graphos PR review intake", cron `0 9 * * *`, active, assignee = cto.
  Satisfies request #1.
- **Merge gate** — routine `27809356-02c5-42f1-9e0a-9ede851d2791`,
  "Daily Graphos merge gate", cron `0 13 * * *`, active, assignee = cto.
  Satisfies request #2: each day at 13:00 it inspects the prior day's PRs and
  merges those that are reviewed and mergable under the Graphos GitFlow / PR
  hygiene contract.

Supporting evidence: sibling issue [AVI-1579](/AVI/issues/AVI-1579) carries the
identical request text and is `done`, consistent with these two routines being
pre-created to satisfy requests #1 and #2.

## Request #3 — Realise policy + routine (NEW; not yet created)

### What "realise" means in Graphos
"Realise" is a **category-theory** term, not a Graphos workflow/OpenSpec term.
It means constructing a concrete datastructure/function that realizes an abstract
universal-construction output — a coproduct, colimit, coequalizer, or lax colimit.

Grounding:
- `openspec/specs/merge-confluence/spec.md:8`: "The merge stage SHALL realise
  `merge(V) = colim D` as the coproduct of a family."
- Math-requirements family: AVI-527 (coproduct), AVI-529 (colimit),
  AVI-537 (local-to-global).
- `formalization/lean/GraphosFormalization/Ext/Lax.lean`: lax-colimit
  realisation semantics.

Operational meaning: whenever a Graphos requirement/formalization asserts the
existence of a universal-construction output, "realise" builds the concrete
computable witness for it.

### Realise policy
- **Scope:** any requirement asserting existence of a coproduct / colimit /
  coequalizer / lax-colimit output that must be computationally effective and
  concretely inhabitable.
- **Trigger sources:** the math-requirements family (AVI-5xx), especially the
  merge-confluence spec (`merge(V) = colim D`) and the Lax colimit
  formalization.
- **Deliverable:** a concrete datastructure + function realizing the abstract
  construction, documented in the formalization and backed by a witness/test.
- **Gate:** until a realisation exists for a construction, that construction's
  requirement stays **`not yet implemented`** (open, blocked on realisation).
  A realised construction flips that requirement toward `done`.

### Realise routine (proposed — NOT yet created)
- **Title:** "Graphos categorical realisation sweep"
- **Assignee:** `category-theory-expert`
  (`08384485-26c8-4de7-80f0-ccae298c012e`) — respects the CTO boundary:
  realisation is category-theory work, not router / authority-of-last-resort
  work.
- **Project:** graphos (`e36e00f3-4e32-47ae-8d2d-69821311fe8e`),
  **goal:** `0914f2c6-5bfe-49c4-9130-92a21b1d44a0`.
- **Schedule:** cron `0 9 * * 1`, Europe/Amsterdam (Mon 09:00) — weekly;
  deep-math work does not need a daily tick.
- **Concurrency:** `coalesce_if_active`; **catch-up:** `skip_missed`.
- **Activity gate:** `require_external_activity`, scope `project` — fires only
  when qualifying graphos-project activity occurred (new/changed math
  requirements or spec), avoiding empty spins.
- **Parent issue:** AVI-1578 (`parentIssueId`) so each realised subtask tracks
  under this planning issue.
- **On fire:** enumerate un-realised categorical constructions across the
  math-requirements family + merge-confluence; for each, spawn a child task
  (assignee `category-theory-expert`) to produce a concrete realisation and flip
  that construction's requirement out of `not yet implemented`; emit a sweep
  summary.

## Implementation order (after confirmation)
1. Create the "Graphos categorical realisation sweep" routine
   (`POST /api/companies/{companyId}/routines`) with the schedule trigger above;
   assignee `category-theory-expert`.
2. Verify the two pre-existing routines (PR intake + merge gate) remain correctly
   configured — no change expected.
3. Decide (optionally on confirmation) whether to fire the new routine once
   manually to seed the first sweep, or let the Monday tick handle it.

## Constraints respected
- **Plan-only:** no routine created until confirmed.
- **CTO role:** the new routine is owned by `category-theory-expert`; the CTO-owned
  routines (intake, merge gate) are left as-is.
- **Branch / PR hygiene:** any future implementation commits stay on
  `AVI-1578-routines`.
