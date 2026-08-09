## 1. Default extraction threads to numCapabilities

- [ ] 1.P Plan: Change `cfgThreads` default from `1` to `GHC.Conc.numCapabilities` in `CLI/Parser.hs`. Check criteria: (1) `graphos .` without `--threads` uses `numCapabilities` threads; (2) `graphos . --threads 1` still works sequentially; (3) `graphos . --threads 8` works with 8 threads. Affected files: `CLI/Parser.hs`, `Domain/Types/Pipeline.hs` (default value). Risk: CI may run on single-core machines where `numCapabilities = 1`.
- [ ] 1.D Do: In `CLI/Parser.hs`, change the `cfgThreads` default from `1` to use `GHC.Conc.numCapabilities`. In `Domain/Types/Pipeline.hs`, update the `Show` instance and any default references. Rebuild.
- [ ] 1.C Check: Run `graphos . --no-viz --no-cluster` with default threads and verify `numCapabilities` appears in the config log. Run with `--threads 1` and verify sequential behavior. Run `cabal test` — all tests pass.
- [ ] 1.A Act: If tests pass, standardize. Update `Domain/Config/Core.hs` if the default needs to be documented there as well.

### Attempt history (1)

<!-- empty -->

## 2. Add tree-sitter grammar FFI bindings

- [ ] 2.P Plan: Add Haskell `tree-sitter-*` package dependencies to `graphos.cabal` and FFI bindings to `Infrastructure/Wiring.getGrammarPtr` for: ruby, java, cpp, nix, markdown. Check criteria: (1) `getGrammarPtr "ruby"` returns `Just`; (2) a `.rb` file produces non-stub nodes; (3) `.nix` file no longer warns; (4) `cabal build` succeeds. Affected files: `graphos.cabal`, `Infrastructure/Wiring.hs`, `Infrastructure/Extract/TreeSitter/Grammar.hs`. Risk: some packages may not exist on Hackage or may have version conflicts.
- [ ] 2.D Do: Add `tree-sitter-ruby`, `tree-sitter-java`, `tree-sitter-cpp`, `tree-sitter-nix`, `tree-sitter-markdown` (or appropriate package names) to `graphos.cabal` build-depends. Add corresponding imports and `getGrammarPtr` entries in `Wiring.hs`. Add the new extensions to `Grammar.knownExtensions` if missing. Rebuild.
- [ ] 2.C Check: Run `graphos . --no-viz --no-cluster` and verify `.rb`, `.java`, `.cpp`/`.hpp`, `.nix`, `.md` files produce meaningful nodes (not stubs). Verify no `[tree-sitter] No grammar for X` warnings for these extensions. Run `cabal test` — all tests pass.
- [ ] 2.A Act: If any tree-sitter package fails to build or has version conflicts, remove that binding, add it to the startup warning list, and document the limitation. Update `Grammar.knownExtensions` to reflect what actually works.

### Attempt history (2)

<!-- empty -->

## 3. Add --timeout CLI flag

- [ ] 3.P Plan: Add `--timeout SECONDS` CLI option that wraps the `runPipeline` call in `System.Timeout.timeout`. Check criteria: (1) `graphos . --timeout 5 --no-viz` exits within 6 seconds with error code 1 and a timeout message; (2) `graphos . --timeout 600` completes normally; (3) no `--timeout` flag runs without limit. Affected files: `CLI/Parser.hs`, `app/Main.hs`. Risk: timeout kills mid-checkpoint, leaving partial state.
- [ ] 3.D Do: Add `cfgTimeout :: Maybe Int` field to `PipelineConfig`. Add `--timeout` parser in `CLI/Parser.hs`. In `app/Main.hs`, wrap `runPipeline` with `timeout (cfgTimeout config * 1000000)` and handle the `Nothing` case (timeout expired) with an error message and `exitWith (ExitFailure 1)`.
- [ ] 3.C Check: Run `graphos . --timeout 5 --no-viz --no-cluster --no-observability` and verify it exits within 6 seconds with error code 1 and a timeout message. Run `graphos . --timeout 600 --no-viz` and verify it completes normally. Run `cabal test`.
- [ ] 3.A Act: Document the `--timeout` flag in the CLI help text. Verify checkpoint preservation on timeout (run with checkpoint, kill with timeout, restart with `--update`).

### Attempt history (3)

<!-- empty -->

## 4. Add extraction progress logging

- [ ] 4.P Plan: Add periodic progress logging to `UseCase/Extract.extractAll` that logs `[extract] Processed X/Y files (Z%)` every 50 files. Check criteria: (1) running `graphos . --verbose` shows progress lines every ~50 files; (2) running with < 50 files shows no progress lines; (3) final "Extracted X nodes, Y edges" summary still appears. Affected files: `UseCase/Extract.hs`. Risk: thread-safety of the counter with concurrent extraction.
- [ ] 4.D Do: Add an `IORef Int` counter in `extractAll`, increment after each file extraction (in each category's extraction loop), and check after increment if `count `mod` 50 == 0` to log progress. Use `atomicModifyIORef'` for thread safety. Compute percentage from total file count.
- [ ] 4.C Check: Run `graphos . --verbose --no-viz --no-cluster` and verify progress lines appear at roughly every 50 files. Run with `--threads 4` and verify progress lines still appear (not garbled). Run `cabal test`.
- [ ] 4.A Act: If progress lines are garbled under concurrency, add a lock or batch the logging. Standardize the progress format.

### Attempt history (4)

<!-- empty -->

## 5. Add startup grammar availability warning

- [ ] 5.P Plan: At the start of `extractAll`, after `partitionByExtractor`, check each tree-sitter grammar name against `getGrammarPtr` and log a single warning listing all missing bindings. Check criteria: (1) removing a grammar binding causes a single startup warning listing the affected extensions; (2) all bindings present produces no warning. Affected files: `UseCase/Extract.hs`, `Infrastructure/Wiring.hs` (need to expose `getGrammarPtr` or add a `knownGrammarNames` function). Risk: minor — just logging.
- [ ] 5.D Do: In `UseCase/Extract.hs`, after partitioning files by extractor, iterate over the `treeSitterFiles` list, collect unique grammar names (via `grammarForFile`), filter those where `epParseWithGrammar` would return `Nothing` (check via `getGrammarPtr`), and log a single warning. Expose `getGrammarPtr` from `Infrastructure.Wiring` if not already exported.
- [ ] 5.C Check: Remove the `"ruby"` binding from `getGrammarPtr` temporarily. Run `graphos . --verbose --no-viz` and verify a single warning listing `.rb` and `ruby`. Restore the binding. Run with all bindings and verify no grammar warning. Run `cabal test`.
- [ ] 5.A Act: Standardize the warning format. Ensure the warning only appears once (not per-file).

### Attempt history (5)

<!-- empty -->