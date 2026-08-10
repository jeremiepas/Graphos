## Context

Graphos extraction currently processes files sequentially by default (`--threads 1`), causing runs on medium-to-large codebases to take 4–10 minutes. The `concurrently` pattern in `extractAll` already supports parallelism via `QSemN`, but the default thread count of 1 negates this. Additionally, several tree-sitter grammars configured in `graphos.yaml` (nix, ruby, java, c++, markdown) have no corresponding FFI binding in `Infrastructure.Wiring.getGrammarPtr`, causing those files to silently fall through to stub extraction (single node, no edges). The Haskell tree-sitter grammar is particularly slow due to its complex AST, amplifying the serial bottleneck. There is no wall-clock timeout, so a hung extraction or unresponsive LSP server can block indefinitely.

## Goals / Non-Goals

**Goals:**
- Default extraction parallelism to `numCapabilities` (GHC RTS number of cores) so parallel extraction works without flags
- Add FFI bindings for all grammars listed in `graphos.yaml` that currently have no `getGrammarPtr` entry (nix, ruby, java, c++, markdown)
- Add `--timeout SECONDS` CLI flag for a wall-clock pipeline timeout
- Add periodic extraction progress logging (every N files) so users can observe progress
- Warn at startup when a configured grammar has no FFI binding

**Non-Goals:**
- Rewriting the extraction pipeline architecture (keep the `concurrently` + `QSemN` approach)
- Adding new language servers or changing LSP connection logic
- Modifying the LSP transport timeout mechanism (already has per-request timeouts)
- Changing the output format or checkpoint system

## Decisions

1. **Default threads = `numCapabilities`**: Use `GHC.Conc.numCapabilities` as the default instead of 1. This matches the GHC RTS default (1 if not overridden, otherwise the `-N` flag value). Users can still set `--threads 1` for debugging. Rationale: most machines have ≥4 cores; parallel extraction of independent files is safe and produces the same results.

2. **Add tree-sitter Haskell bindings for 5 new grammars**: Add `tree-sitter-ruby`, `tree-sitter-java`, `tree-sitter-cpp`, `tree-sitter-nix`, `tree-sitter-markdown` to cabal dependencies and `getGrammarPtr`. These are all available on Hackage as `tree-sitter-ruby`, etc. Alternative considered: use only stubs for these languages — rejected because stubs produce useless single-node extractions.

3. **`--timeout` as a `System.Timeout.timeout` wrapper**: Wrap the entire `runPipeline` call in `timeout (seconds * 1000000)`. This is the simplest, most reliable approach — it kills the whole process if the pipeline exceeds the limit. Alternative: per-stage timeouts with checkpoint-and-resume — too complex for this change.

4. **Progress logging every 50 files**: Add a counter in `extractAll` that logs `"[extract] Processed X/Y files (Z%)"` every 50 files. This is minimal overhead and gives clear feedback. Alternative: percentage-based logging — harder to compute accurately with concurrent extraction.

5. **Startup grammar warning in `extractAll`**: After `partitionByExtractor`, check if any `tree-sitter` files will use `getGrammarPtr` that returns `Nothing`, and log a warning listing the affected extensions. This replaces the per-file `[tree-sitter] No grammar for X` pattern with a single startup summary.

## Risks / Trade-offs

- **[New tree-sitter package availability]**: Some Haskell tree-sitter bindings (especially `tree-sitter-nix`) may have limited maintenance or version incompatibility with the `tree-sitter` package. → **Mitigation**: Pin compatible versions in `cabal.project`; fall back to stub if the package fails to build (guarded by CPP macro or conditional flag).
- **[Thread safety of tree-sitter FFI]**: Tree-sitter's C library uses global state for parser allocation. Parallel parsing from multiple Haskell threads could race. → **Mitigation**: The `tree-sitter` Haskell package already uses `withParser` which creates a fresh parser per call, so each FFI call is independent. Verify with tests.
- **[numCapabilities = 1 in single-threaded RTS]**: If the user runs with `+RTS -N1` or doesn't pass `-N`, `numCapabilities` will be 1. This is acceptable — it's the current behavior. Users who want parallelism must use `-N` or `--threads`.
- **[Timeout killing mid-export]**: The `--timeout` wrapper kills the entire pipeline, potentially leaving partial output files. → **Mitigation**: The existing checkpoint system handles this — on restart, the pipeline resumes from the last checkpoint. Document this behavior.

## Verification Strategy (Check)

1. **Parallelism**: Run `graphos . --no-viz --no-cluster` with default settings on the Graphos codebase; verify extraction completes in <2min (down from >4min). Run with `--threads 1` and verify it's slower.
2. **New grammars**: Run `graphos . --no-viz --no-cluster` and verify `.rb`, `.java`, `.cpp`, `.nix`, `.md` files produce nodes with `nodeKind != "Module"` (i.e., they're actually parsed, not stubbed).
3. **Startup warning**: Remove a grammar binding, run graphos, verify a single warning listing the affected extensions appears at startup.
4. **Timeout**: Run `graphos . --timeout 5 --no-viz` and verify the process exits with a timeout error within 6 seconds.
5. **Progress logging**: Run with `--verbose` and verify `"[extract] Processed X/Y files"` appears every 50 files.
6. **Existing tests**: `cabal test` must pass with no regressions.

## Iteration & Rollback (Act)

- If new tree-sitter packages cause build failures on any platform: remove the offending package from `getGrammarPtr` and cabal, add it to the startup warning list, and iterate.
- If `numCapabilities` default causes issues on CI or low-core machines: add a `--threads` override in CI scripts, or cap the default at `max 1 (min 4 numCapabilities)`.
- If `--timeout` causes checkpoint corruption: document the `--update` flag for resume, and ensure checkpoint is written before the timeout kills the process.
- Learnings from this change feed into: (1) potential future per-file extraction timeouts, (2) extraction performance benchmarking in CI.