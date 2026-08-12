<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Extract viewer assets and vendor the renderer — PLAN

**Task slug**: `04-extract-viewer-assets-and-vendor-the-renderer`
**Attempt**: 1
**Status**: pending

## Summary

Move viewer CSS/JS from Haskell string literals to embedded asset files and vendor the vis-network renderer.

## Detail

**PLAN**:

**Scope**:
- Move CSS from `src/Graphos/Infrastructure/Export/HTML.hs:69–120` to `assets/viewer/viewer.css`.
- Move JS from `src/Graphos/Infrastructure/Export/HTML.hs:176–804` to `assets/viewer/viewer.js`.
- Vendor `vis-network.min.js` to `assets/viewer/vis-network.min.js`.
- Embed all three using `file-embed`.
- Remove CDN script and `_visLoadFailed` path from `HTML.hs`.
- Consolidate the three `options` blocks into one base object with overrides.
- Move `hideEdgesOnDrag`/`hideEdgesOnZoom` from `physics` to `interaction`.
- Add missing CSS rules for `.search-verdict`, `.search-suggestions`, and `.result-item.scored`.
- Update `graphos.cabal` with `file-embed` and `extra-source-files`.

**Check Criteria**:
- **No string literals**: Grep `HTML.hs` for JS/CSS content; none should remain.
- **Byte-identity**: Emitted assets are byte-identical to their source files.
- **No external origins**: No `http://` or `https://` in `src`/`href` of emitted document.
- **Offline rendering**: Document renders correctly when opened from `file://` with networking disabled.
- **Renderer version**: The version of the vendored renderer is recorded in the document.
- **Single options**: Exactly one renderer options definition exists in the source.
- **Interaction**: `hideEdgesOnDrag` and `hideEdgesOnZoom` work correctly in the `interaction` section.
- **Compilation**: `cabal build --flag dev` and `cabal test` pass.

**Affected Modules**:
- `src/Graphos/Infrastructure/Export/HTML.hs`
- `graphos.cabal`
- `assets/viewer/` (new directory)

**Prerequisites**:
- `file-embed` package availability.

**Risks**:
- Increased build time due to embedding large assets.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
