<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Extract viewer assets and vendor the renderer — DO

**Task slug**: `04-extract-viewer-assets-and-vendor-the-renderer`
**Attempt**: 1
**Status**: pending

## Summary

Extracted viewer CSS and JS into asset files, vendored vis-network, and embedded them using `file-embed`.

## Detail

**DO**:

- **Asset Extraction**:
  - Created `assets/viewer/viewer.css` and populated it with the CSS extracted from `HTML.hs:69–120`.
  - Created `assets/viewer/viewer.js` and populated it with the JS extracted from `HTML.hs:176–804`.
  - Vendored `vis-network.min.js` into `assets/viewer/vis-network.min.js` and included its license.
- **Embedding**:
  - Added `file-embed` dependency to `graphos.cabal`.
  - Updated `graphos.cabal` with `extra-source-files` for the new assets.
  - Refactored `src/Graphos/Infrastructure/Export/HTML.hs` to use `embedFile` for the CSS, JS, and vendor bundle.
  - Removed the `<script>` tag for the unpinned CDN and the `_visLoadFailed` logic.
- **Refactoring `HTML.hs`**:
  - Consolidated the three `options` blocks into a single `baseOptions` object.
  - Created named overrides for each depth level (`overviewOptions`, `communityOptions`, `fullOptions`, `customOptions`).
  - Moved `hideEdgesOnDrag` and `hideEdgesOnZoom` from the `physics` section to the `interaction` section.
  - Added the missing CSS rules for `.search-verdict`, `.search-suggestions`, and `.result-item.scored` to the embedded stylesheet.
- **Documentation**:
  - Recorded the vendored version of `vis-network` in the emitted HTML.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
