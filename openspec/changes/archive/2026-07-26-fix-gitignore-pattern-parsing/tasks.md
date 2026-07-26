## 1. Add WildcardPattern constructor and fnmatch matcher

Add the `WildcardPattern` constructor to `IgnorePattern` and implement the `fnmatch`-style matching function that handles `*` (single-segment) and `**` (recursive) wildcards.

- [x] 1.P Plan: Add `WildcardPattern String` to `IgnorePattern` in `Ignore.hs`. Implement `wildcardMatch :: String -> String -> Bool` that splits pattern on `*`/`**` and matches segments in order. Update `matches` to dispatch `WildcardPattern` to `wildcardMatch`. Check: all existing tests pass, new `WildcardPattern` patterns can match paths with `*` in any position.
- [x] 1.D Do: Add `WildcardPattern String` constructor to `IgnorePattern` deriving `Eq, Show`. Implement `wildcardMatch` as a pure function that: (1) normalizes `**` to a sentinel, (2) splits pattern on `*`, (3) matches each segment in order within the path, (4) `**` matches across `/` boundaries. Add `matches path (WildcardPattern p) = wildcardMatch p path` case. Update exports to include `WildcardPattern`.
- [x] 1.C Check: `cabal build` succeeds. All existing `IgnoreSpec` tests pass. Manually verify in REPL: `wildcardMatch "result-*" "result-1" == True`, `wildcardMatch ".ghc.environment.*" ".ghc.environment.x86_64-linux" == True`, `wildcardMatch "*.log" "debug.log" == True`.
- [x] 1.A Act: If build fails, fix type errors (exhaustive pattern match on `IgnorePattern` in any `case` expressions). If tests fail, check that `GlobPattern` still works for existing hardcoded patterns.

## 2. Fix parsePattern classification logic

Restructure `parsePattern` to check for `*` wildcards first, then apply the correct heuristic for remaining patterns.

- [x] 2.P Plan: Rewrite `parsePattern` with priority: (1) `*` anywhere → `WildcardPattern`, (2) trailing `/` → `PrefixPattern`, (3) leading `/` → `ExactPattern`, (4) simple filename (no `/`, no `*`) → `ExactPattern`, (5) other → `ExactPattern`. Remove the `'.' \`elem\` trimmed → SuffixPattern` rule. Check: `.ghc.environment.*` → `WildcardPattern`, `result-*` → `WildcardPattern`, `cabal.project.local` → `ExactPattern`, `node_modules/` → `PrefixPattern`, `*.log` → `WildcardPattern`.
- [x] 2.D Do: Rewrite `parsePattern` with the new classification order. Keep `GlobPattern` as a legacy constructor but stop producing it from `parsePattern` — change the `(' '*':rest)` case to produce `WildcardPattern` instead. Remove the `SuffixPattern` production for dot-containing strings that are actually exact filenames. Keep `SuffixPattern` for true suffix patterns if needed, but audit all usages.
- [x] 2.C Check: `cabal build` succeeds. All existing `IgnoreSpec` tests pass. Verify new parsing: `parsePattern ".ghc.environment.*" == WildcardPattern ".ghc.environment.*"`, `parsePattern "result-*" == WildcardPattern "result-*"`, `parsePattern "cabal.project.local" == ExactPattern "cabal.project.local"`, `parsePattern "node_modules/" == PrefixPattern "node_modules"`, `parsePattern "*.log" == WildcardPattern "*.log"`.
- [x] 2.A Act: If existing tests break due to `GlobPattern` vs `WildcardPattern` changes, update test expectations to use `WildcardPattern`. If `SuffixPattern` removal breaks things, identify which patterns genuinely need suffix semantics and add a targeted rule.

## 3. Update hardcodedIgnorePatterns and tests for new pattern types

Update `hardcodedIgnorePatterns` and `loadIgnorePatterns` to produce correct pattern types. Update test expectations to match new classification.

- [x] 3.P Plan: Audit `hardcodedIgnorePatterns` to verify all hardcoded directory names still produce correct `IgnorePattern` variants. Update `IgnoreSpec` test expectations where `GlobPattern` was expected — change to `WildcardPattern`. Add new test cases for wildcard patterns, `**` patterns, dot-filenames, and negation with wildcards. Check: all tests pass, project `.gitignore` loads correctly.
- [x] 3.D Do: In `hardcodedIgnorePatterns`, note that entries are plain directory names (no `*`), so they will now produce `ExactPattern` instead of `SuffixPattern` for dot-containing names like `.git`, `.next`, `.gradle`. Update test expectations in `hardcodedIgnorePatterns` spec: `SuffixPattern ".git"` → `ExactPattern ".git"`, `SuffixPattern ".next"` → `ExactPattern ".next"`, `SuffixPattern ".gradle"` → `ExactPattern ".gradle"`. Add 15+ new test cases covering: `WildcardPattern` matching, `**` recursive matching, dot-filename exact matching, negation with wildcards, full `.gitignore` loading integration test.
- [x] 3.C Check: `cabal test` passes. Manually trace each line of the project's `.gitignore` through `loadGitignore` to verify correct parsing. Verify `shouldIgnore` correctly excludes `.ghc.environment.x86_64-linux`, `result-1`, `result-foo`, and correctly includes `.opencode/agent/core/openagent.md` when negation is applied.
- [x] 3.A Act: If any hardcoded pattern type change breaks matching behavior, verify the `matches` function for `ExactPattern` still handles the directory-entry check correctly (the `isInfixOf` + path boundary check). Adjust `matches` for `ExactPattern` if needed to maintain backward compatibility.

## 4. Integration test with project .gitignore

Create an integration test that loads the project's own `.gitignore` and verifies every line produces the correct pattern type and matches intended paths.

- [x] 4.P Plan: Write a test that calls `loadGitignore` with the project root directory and verifies: (1) every line is parsed without error, (2) specific lines produce expected pattern types, (3) `shouldIgnore` returns correct results for representative paths. Check: full integration test passes.
- [x] 4.D Do: Add an integration test in `IgnoreSpec.hs` that loads `.gitignore` from the project root (or a temp file with the project's gitignore content). Verify key patterns: `.ghc.environment.*` → `WildcardPattern`, `result` and `result-*` → `ExactPattern` and `WildcardPattern`, `.tmp/` → `PrefixPattern`, `.opencode/*` → `WildcardPattern`, negation patterns. Test that `shouldIgnore` correctly excludes `.ghc.environment.x86_64-linux`, `result-1`, `.tmp/sessions/`, and correctly includes `.opencode/agent/` when negation is applied.
- [x] 4.C Check: `cabal test` passes. Integration test covers all `.gitignore` line types. `cabal run graphos -- .` does not include `.ghc.environment.*` or `result-*` files in detection output.
- [x] 4.A Act: If integration test reveals patterns that still don't match correctly, add them as test cases and fix `parsePattern` or `wildcardMatch` as needed.