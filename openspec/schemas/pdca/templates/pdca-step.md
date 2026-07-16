<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task <N> — <task name> — <PLAN | DO | CHECK | ACT>

**Task slug**: `<NN-task-slug>`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

<!-- One or two lines: what this step covers for THIS task. -->

## Detail

<!--
  PLAN  : scope of this task, Check Criteria (defined BEFORE code: what tests,
          what spec scenarios, what PASS/FAIL boundaries), affected modules,
          prerequisites, risks. No code.
  DO    : what was implemented, key decisions, concrete changes.
  CHECK : execute Check Criteria from plan.md verbatim; PASS/FAIL per criterion
          with evidence (commands, outputs). Do NOT invent new criteria. If a
          Plan criterion is unclear, record that — do not redefine to pass.
  ACT   : standardized outcome if OK; if NOT OK, the reason + link to the new
          attempt folder (attempt-N/).
-->

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
