<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. <!-- task name -->

<!-- One task = one PDCA cycle. Steps run in order; mark [x] as completed. -->

- [ ] 1.P Plan: <!-- scope of THIS task + Check criteria (what proves success, defined before code), affected areas, risks -->
- [ ] 1.D Do: <!-- implement THIS task -->
- [ ] 1.C Check: <!-- execute Check criteria from 1.P; record PASS/FAIL per criterion (do NOT invent new criteria) -->
- [ ] 1.A Act: <!-- standardize what worked; address what Check surfaced -->

### Attempt history (1)

<!-- Leave empty if the first attempt passes.
     If Act is NOT OK, append a dated note for the failed attempt here, then add
     a new cycle below (1.P attempt 2 → 1.A attempt 2). Never delete prior notes.
- attempt 1 (YYYY-MM-DD): Act NOT OK — <reason>. Starting attempt 2.
-->

## 2. <!-- task name -->

- [ ] 2.P Plan: <!-- scope + Check criteria before code -->
- [ ] 2.D Do: <!-- ... -->
- [ ] 2.C Check: <!-- execute criteria from 2.P; no inventing -->
- [ ] 2.A Act: <!-- ... -->

### Attempt history (2)

<!-- empty unless a retry is needed -->
