## Why

Graphos extraction is slow and has missing tree-sitter grammar coverage. Running `graphos .` on its own codebase (488 files) takes 4–10 minutes because extraction defaults to a single thread and several configured grammars (nix, ruby, java, markdown, etc.) have no FFI binding, causing files to fall through to stub extraction with no useful nodes. The `--threads` flag exists but defaults to 1, and missing grammars silently produce empty stubs instead of meaningful parse results.

## What Changes

- **Default threads to `numCapabilities`** (number of CPU cores) instead of 1, so extraction parallelism works out-of-the-box
- **Add missing tree-sitter grammar FFI bindings** for nix, ruby, java, c++, markdown, and other configured-but-unbound languages in `Wiring.getGrammarPtr`
- **Add a `--timeout` CLI flag** to set a wall-clock timeout for the full pipeline, so it fails fast instead of hanging indefinitely
- **Log extraction progress** with a periodic status line (e.g., every 50 files) so users know extraction is progressing, not stuck
- **Warn on missing grammar at config-load time** rather than silently falling through to stub per-file

## Capabilities

### New Capabilities
- `extraction-timeout`: Wall-clock timeout for the extraction pipeline stage, allowing the process to fail fast when extraction hangs or takes too long
- `progress-logging`: Periodic extraction progress reporting (files processed / total) so users can observe that extraction is proceeding

### Modified Capabilities
- `extraction`: Add parallel extraction default (use `numCapabilities` threads), add missing tree-sitter grammar FFI bindings (nix, ruby, java, c++, markdown, etc.), and warn at startup when a configured grammar has no FFI binding

## Impact

- **Code**: `UseCase/Extract.hs` (parallel default), `Infrastructure/Wiring.hs` (grammar bindings), `Infrastructure/Extract/TreeSitter/Grammar.hs` (known extensions), `Infrastructure/Extract/TreeSitter/Core.hs` (possible timeout per-file), `CLI/Parser.hs` (--timeout flag), `UseCase/Pipeline.hs` (timeout wrapper)
- **Dependencies**: New `tree-sitter-ruby`, `tree-sitter-java`, `tree-sitter-cpp`, `tree-sitter-nix`, `tree-sitter-markdown` Haskell packages needed in cabal file
- **API**: New `--timeout` CLI flag (non-breaking)
- **Behavior**: Extraction runs faster by default; previously-silent grammar misses now produce startup warnings

## PDCA Cycle

- **Plan**: Reduce extraction time on the Graphos codebase from >4min to <2min with default settings; ensure zero silent stub fallbacks for configured grammars
- **Do**: Default threads to numCapabilities, add grammar FFI bindings, add --timeout flag, add progress logging
- **Check**: Run `graphos . --no-viz --no-cluster` before and after; measure wall-clock time; verify no "No grammar" warnings for previously-configured languages; verify --timeout kills the process within the specified bound
- **Act**: If extraction time improves ≥2x and no silent stubs remain, standardize; if new grammar packages have build issues, iterate on package versions