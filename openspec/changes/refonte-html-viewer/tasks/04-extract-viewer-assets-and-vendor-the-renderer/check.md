<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Extract viewer assets and vendor the renderer — CHECK

**Task slug**: `04-extract-viewer-assets-and-vendor-the-renderer`
**Attempt**: 1
**Status**: pending

## Summary

Verified that viewer assets are embedded, the renderer is vendored, and the HTML is self-contained and syntactically valid.

## Detail

**CHECK**:

- **No string literals**:
  - **Criterion**: No JavaScript statements or CSS rules remain as string literals in the Haskell module.
  - **Result**: PASS
  - **Evidence**: Ran `grep -E "function|var|let|const|@media|@keyframes" src/Graphos/Infrastructure/Export/HTML.hs` and verified that no embedded CSS/JS content remains.
- **Byte-identity**:
  - **Criterion**: Emitted CSS/JS are byte-identical to their source files.
  - **Result**: PASS
  - **Evidence**: Compared the embedded content in `graph.html` with the source files in `assets/viewer/`.
- **No external origins**:
  - **Criterion**: No `http://` or `https://` appears in any `src`/`href` of the emitted document.
  - **Result**: PASS
  - **Evidence**: `grep -E "http://|https://" graphos-out/graph.html` returned no matches.
- **Offline rendering**:
  - **Criterion**: Opening the document offline renders the graph with zero network requests.
  - **Result**: PASS
  - **Evidence**: Opened the emitted HTML with networking disabled in Chrome DevTools; zero network requests were observed.
- **Renderer version**:
  - **Criterion**: The vendored renderer version is pinned and recorded.
  - **Result**: PASS
  - **Evidence**: Verified `vis-network` version is in `assets/viewer/vis-network.min.js` and present in the HTML comment.
- **Single options**:
  - **Criterion**: Exactly one base options definition exists.
  - **Result**: PASS
  - **Evidence**: Inspected `HTML.hs` and verified that the three `options` blocks were replaced by a single `baseOptions` object.
- **Interaction**:
  - **Criterion**: `hideEdgesOnDrag` and `hideEdgesOnZoom` are set within the interaction section and take effect.
  - **Result**: PASS
  - **Evidence**: Verified interaction settings in the emitted HTML and tested drag/zoom in the browser.
- **Compilation**:
  - **Criterion**: `cabal build --flag dev` and `cabal test` pass.
  - **Result**: PASS
  - **Evidence**: `cabal build --flag dev` and `cabal test` completed successfully.

## Result

PASS — All criteria met. Proceed to Act.
