## Why

The `.gitignore` pattern parser in `Graphos.Infrastructure.FileSystem.Ignore` mishandles several common `.gitignore` syntax patterns, causing files that should be ignored to be processed during extraction. The `parsePattern` function only handles `*` at the start of a line, incorrectly classifies dot-containing filenames as suffix patterns, and fails to match patterns like `.ghc.environment.*` and `result-*`. This means Graphos may index build artifacts, temp files, and other files that should be excluded — polluting the knowledge graph and wasting LLM tokens.

## What Changes

Rewrite `parsePattern` to correctly handle the full subset of `.gitignore` syntax that Graphos supports:

1. **Wildcard-in-middle patterns** (e.g., `.ghc.environment.*`, `result-*`) — currently produce wrong pattern types that never match
2. **Dot-containing exact filenames** (e.g., `cabal.project.local`, `.DS_Store`) — currently parsed as `SuffixPattern` instead of `ExactPattern`
3. **Double-star patterns** (e.g., `!.opencode/agent/**`) — `**` is treated as literal text
4. **Trailing glob after slash** (e.g., `.opencode/*`) — `*` after `/` not handled as glob

The fix introduces a `WildcardPattern` type for patterns containing `*` in any position (not just leading), and corrects the classification logic so filenames with dots but no wildcards become `ExactPattern`.

## Capabilities

### Modified Capabilities
- `gitignore-parsing`: Fix pattern classification so all common `.gitignore` patterns match correctly, ensuring files meant to be excluded are actually excluded during detection

## Impact

- **Code**: `src/Graphos/Infrastructure/FileSystem/Ignore.hs` — rewrite `parsePattern`, add `WildcardPattern` constructor, update `matches`
- **Code**: `src/Graphos/UseCase/Detect.hs` — `hardcodedIgnoreDirNames` already works correctly (fast path), no change needed
- **Tests**: `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs` — add comprehensive tests for previously-broken patterns
- **No API changes**: Exported types change (`IgnorePattern` gains a constructor), but all consumers are internal

## PDCA Cycle

- **Plan**: All `.gitignore` patterns in the project's own `.gitignore` file must be correctly parsed and matched by `shouldIgnore`. Success = every line in `.gitignore` produces the correct `IgnorePattern` variant and matches the intended file paths.
- **Do**: Rewrite `parsePattern` with proper wildcard handling, add `WildcardPattern` constructor, update `matches` function, add comprehensive test coverage for edge cases.
- **Check**: Run `cabal test` — all existing tests pass + new tests covering `.ghc.environment.*`, `result-*`, `cabal.project.local`, `.opencode/*`, `!.opencode/agent/**` patterns. Verify that the project's own `.gitignore` lines produce correct matches.
- **Act**: If successful, consider upstreaming the improved `.gitignore` parser as a standalone library. Monitor for any new `.gitignore` patterns added by users that reveal further edge cases.