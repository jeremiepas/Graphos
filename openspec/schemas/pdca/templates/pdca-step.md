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
  DO    : implementation plan (created BEFORE code): what WILL be implemented,
          concrete changes, key decisions, approach. Updated after code to
          reflect what was actually done (deviations from plan).
  CHECK : verification plan (created BEFORE code): how the implementation WILL
          be verified, based on plan.md criteria. Updated after code with
          actual results: commands run, outputs, PASS/FAIL per criterion.
  ACT   : final verdict trace (generated AFTER implementation+checking):
          summarizes the whole cycle outcome. If OK → PASS. If NOT OK → FAIL,
          reason + link to attempt-N/ for a new cycle.
-->

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
