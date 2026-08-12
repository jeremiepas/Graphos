<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 9 — Split UseCase.Extract into focused sub-modules — DO

**Task slug**: `09-split-usecase-extract`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Split `UseCase.Extract` (574 lines) into `UseCase.Extract.Core` (orchestration), `UseCase.Extract.LSP` (LSP workflow), and `UseCase.Extract.TreeSitter` (TreeSitter fallback). Original becomes backward-compatible re-export.

## Detail

### What needs to be implemented

1. **Create `UseCase.Extract.Core`** — contains `extractAll` function signature and core orchestration logic. Delegates to port methods for all IO. No Infrastructure imports.

2. **Create `UseCase.Extract.LSP`** — contains LSP-specific extraction workflow:
   - `extractFilesWithLSP` — orchestrate LSP extraction for a file list
   - `extractWorkspaceSymbols` — extract workspace-level symbols
   - LSP lifecycle management (find server, connect, extract, disconnect)

3. **Create `UseCase.Extract.TreeSitter`** — contains TreeSitter-specific extraction:
   - `extractViaTreeSitterFFI` — extract using tree-sitter grammars
   - Grammar resolution

4. **Convert `UseCase.Extract.hs`** to a re-export module (<30 lines):
   ```haskell
   module Graphos.UseCase.Extract
     ( module Graphos.UseCase.Extract.Core
     , module Graphos.UseCase.Extract.LSP
     , module Graphos.UseCase.Extract.TreeSitter
     , module Graphos.UseCase.Extract.Image
     , module Graphos.UseCase.Extract.Office
     , module Graphos.UseCase.Extract.Markdown
     , module Graphos.UseCase.Extract.Haskell
     ) where
   import Graphos.UseCase.Extract.Core
   import Graphos.UseCase.Extract.LSP
   import Graphos.UseCase.Extract.TreeSitter
   import Graphos.UseCase.Extract.Image
   import Graphos.UseCase.Extract.Office
   import Graphos.UseCase.Extract.Markdown
   import Graphos.UseCase.Extract.Haskell
   ```

5. **Add new modules to `.cabal` file**

### Prerequisites

- Task 6 must be complete (Extract uses ports, no Infrastructure imports)

### Concrete changes needed

- Create `src/Graphos/UseCase/Extract/Core.hs` — orchestration with port-delegated IO
- Create `src/Graphos/UseCase/Extract/LSP.hs` — LSP workflow
- Create `src/Graphos/UseCase/Extract/TreeSitter.hs` — TreeSitter workflow
- Convert `src/Graphos/UseCase/Extract.hs` to re-export module
- Update `.cabal` file with new exposed-modules
- Verify all existing imports still compile

## Result

NOT YET IMPLEMENTED — awaiting Do phase.