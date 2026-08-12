<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Extract viewer assets and vendor the renderer — ACT

**Task slug**: `04-extract-viewer-assets-and-vendor-the-renderer`
**Attempt**: 1
**Status**: PASS

## Summary

The viewer assets have been successfully extracted, vendored, and embedded, making the HTML output genuinely self-contained.

## Detail

**ACT**:

The implementation of Task 4 is complete and has passed all Check criteria.

- **Standardized Outcome**: PASS.
- **Implementation Summary**:
  - CSS and JavaScript are now managed as separate source files in `assets/viewer/`.
  - `vis-network` is vendored and embedded, removing the dependency on `unpkg.com`.
  - The HTML document is now fully self-contained and works offline from `file://`.
  - The code in `HTML.hs` is cleaner and more maintainable due to the removal of large string literals.
  - The interaction settings and options consolidation have improved the robustness of the viewer.

## Result

PASS — Task 4 is complete. Proceeding to Task 5.
