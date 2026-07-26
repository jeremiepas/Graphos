## Context

Graphos's `Infrastructure.FileSystem.Ignore` module parses `.gitignore` and `.graphosignore` files to filter out unwanted files during the detection phase of the pipeline. The current `parsePattern` function uses a simplified heuristic that misclassifies several common `.gitignore` patterns:

| `.gitignore` line | Current parse | Expected behavior | Bug |
|---|---|---|---|
| `.ghc.environment.*` | `SuffixPattern ".ghc.environment.*"` | Match paths starting with `.ghc.environment.` | Wildcard `*` in middle not handled |
| `result-*` | `ExactPattern "result-*"` | Match `result-1`, `result-foo` | `*` in middle treated as literal |
| `cabal.project.local` | `SuffixPattern "cabal.project.local"` | Exact filename match | Dot-containing names become suffixes |
| `.opencode/*` | `SuffixPattern ".opencode/*"` | Match files inside `.opencode/` | `*` after `/` not handled as glob |
| `!.opencode/agent/**` | `NegatePattern (GlobPattern ".opencode/agent/")` | Re-include `.opencode/agent/` recursively | `**` not handled |

The `hardcodedIgnoreDirNames` fast path in `Detect.hs` masks many of these issues for directory-level patterns (like `.opencode`, `.tmp`, `node_modules`), but file-level patterns and wildcard patterns are genuinely broken.

## Goals / Non-Goals

**Goals:**
- Correctly parse all `.gitignore` patterns present in the project's own `.gitignore` file
- Support `*` wildcard in any position (start, middle, end) — not just leading `*`
- Distinguish between exact filenames (with dots) and suffix patterns
- Handle `**` (double-star) as recursive directory wildcard
- Maintain backward compatibility with existing `hardcodedIgnoreDirNames` fast path
- Maintain the priority system (hardcoded=0 < gitignore=1 < graphosignore=2) and negation

**Non-Goals:**
- Full `.gitignore` spec compliance (e.g., `[^x]` negation classes, `?` single-char wildcards, range patterns)
- Handling per-directory `.gitignore` files (only root `.gitignore` is read)
- Handling leading `/` as root-relative anchoring (current behavior is adequate for Graphos's use case)
- Changing the `AnnotatedPattern` or `NegatePattern` types (they work correctly)
- Changing the `hardcodedIgnoreDirNames` list in `Detect.hs` (the fast path is correct)

## Decisions

### Decision 1: Replace `GlobPattern` semantics and add `WildcardPattern`

**Choice**: Extend `IgnorePattern` with a new `WildcardPattern String` constructor that stores the original pattern with `*` wildcards intact, and change `GlobPattern` to only mean "suffix match" (its current semantic for `*.ext` patterns). The `matches` function will handle `WildcardPattern` by converting `*` to a simple glob matcher.

**Alternatives considered**:
- **A: Use a regex library (regex-tdfa)** — Adds a dependency for a narrow use case. Overkill for simple glob matching. Rejected: dependency cost too high.
- **B: Replace all pattern types with a single compiled regex** — Loses type-level clarity and makes debugging harder. Rejected: over-engineered.
- **C: Simple string matching with `*` as wildcard** — Implement `fnmatch`-style matching in pure Haskell. No new dependencies. Matches the scope of `.gitignore` patterns Graphos actually encounters. **Selected**.

**Rationale**: The current type system (`PrefixPattern`, `SuffixPattern`, `ExactPattern`, `GlobPattern`) maps well to the most common `.gitignore` patterns. Adding `WildcardPattern` handles the edge cases (`result-*`, `.ghc.environment.*`, `**`) without disrupting the existing fast paths. A simple `fnmatch`-style matcher (split on `*`, match segments in order) is ~10 lines and needs no dependencies.

### Decision 2: Fix `parsePattern` classification logic

**Choice**: Restructure `parsePattern` to check for `*` wildcards first, then classify remaining patterns:

1. If line contains `*` → `WildcardPattern` (preserve original pattern, `*` matched as any chars)
2. If line ends with `/` → `PrefixPattern` (directory match, strip trailing `/`)
3. If line starts with `/` → `ExactPattern` (anchored path)
4. If line contains `.` AND is a simple filename (no `/`) → `ExactPattern` (exact filename match)
5. Otherwise → `ExactPattern` (directory or filename match)

**Alternatives considered**:
- **A: Keep current logic but fix the `*`-in-middle case only** — Leaves the dot-filename issue. Incomplete fix. Rejected.
- **B: Full `.gitignore` parser** — Too much scope. Graphos only reads the root `.gitignore` and doesn't need per-directory or anchored patterns. Rejected.
- **C: Restructure classification with wildcard-first** — Clean, handles all current `.gitignore` lines correctly, and is extensible for future patterns. **Selected**.

**Rationale**: The restructured logic correctly handles all patterns in the project's `.gitignore` while staying simple. The wildcard check first ensures patterns like `.ghc.environment.*` and `result-*` are caught before they fall through to suffix/exact classification.

### Decision 3: Handle `**` as recursive match

**Choice**: In the `WildcardPattern` matcher, treat `**` as matching zero or more path segments (i.e., `/` inclusive), and `*` as matching zero or more characters within a single segment (i.e., `/` exclusive).

**Alternatives considered**:
- **A: Treat `**` same as `*`** — Loses the recursive semantics. `!.opencode/agent/**` wouldn't properly re-include nested paths. Rejected.
- **B: Normalize `**` away at parse time** — Could convert `dir/**` to `PrefixPattern "dir/"`. But `**` can appear in middle of patterns. Rejected: too lossy.
- **C: Implement `fnmatch`-style with `**` awareness** — Split pattern on `/`, handle `**` segments as "match any depth". More complex but correct. **Selected**.

**Rationale**: The `fnmatch`-style approach with `**` awareness correctly handles the `.opencode/*` and `!.opencode/agent/**` patterns in the project's `.gitignore`. The implementation is ~20 lines of pure Haskell with no dependencies.

### Decision 4: Keep `hardcodedIgnoreDirNames` fast path unchanged

**Choice**: Do not modify `hardcodedIgnoreDirNames` in `Detect.hs`. The fast-path name check (`entry \`elem\` hardcodedIgnoreDirNames`) is correct for directory-name matching and doesn't depend on `parsePattern`.

**Alternatives considered**:
- **A: Merge hardcoded patterns into `WildcardPattern`** — Would slow down the fast path. Rejected.
- **B: Remove `hardcodedIgnoreDirNames` and rely solely on file-based patterns** — Would break the fast check and make directory traversal slower. Rejected.

**Rationale**: The fast path works correctly. No change needed.

## Risks / Trade-offs

- **[Risk: WildcardPattern matcher is too broad]** → Mitigation: Test suite covering all `.gitignore` patterns in the project plus edge cases. `*` only matches within a path segment (not across `/`), `**` matches across segments.
- **[Risk: Changing parsePattern breaks existing hardcoded pattern types]** → Mitigation: All existing `IgnorePattern` constructors preserved. `WildcardPattern` is additive. Existing tests must still pass unchanged.
- **[Risk: Performance regression from wildcard matching]** → Mitigation: `WildcardPattern` only applies to patterns containing `*` — the fast `PrefixPattern`/`SuffixPattern`/`ExactPattern` paths are unchanged. Wildcard matching is only called when a path doesn't match the fast paths.
- **[Risk: `**` semantics differ from git's]** → Mitigation: Graphos reads only the root `.gitignore`, so per-directory `**` behavior doesn't apply. The implementation handles the patterns present in the project's own `.gitignore` correctly.

## Verification Strategy (Check)

1. **Unit tests** — Add 15+ test cases to `IgnoreSpec.hs` covering:
   - `WildcardPattern` matching: `result-*` matches `result-1`, `result-foo`; `.ghc.environment.*` matches `.ghc.environment.x86_64-linux`
   - `**` recursive matching: `!.opencode/agent/**` re-includes `.opencode/agent/core/openagent.md`
   - Dot-filename classification: `cabal.project.local` parsed as `ExactPattern`, not `SuffixPattern`
   - Backward compatibility: All existing tests pass unchanged
2. **Integration test** — Load the project's own `.gitignore` file via `loadGitignore` and verify every line produces the correct `IgnorePattern` variant and matches intended paths
3. **Property test** — QuickCheck property: for any pattern without `*`, the new `parsePattern` produces the same result as the old logic
4. **Build verification** — `cabal build` and `cabal test` pass with no warnings or failures

## Iteration & Rollback (Act)

- **If tests fail**: Fix the `parsePattern` logic incrementally. The `WildcardPattern` addition is additive — if wildcard matching is buggy, we can narrow its scope to only patterns that the old logic couldn't handle.
- **If performance degrades**: Profile `shouldIgnore` calls. Consider caching parsed patterns or adding a bloom filter for fast rejection.
- **If new `.gitignore` patterns are encountered**: Add them to the test suite as regression tests. Consider extracting the parser into a standalone utility module if the pattern list grows beyond 30 variants.
- **Rollback**: Revert `IgnorePattern` and `parsePattern` changes. The old behavior is preserved in git history. The `WildcardPattern` constructor addition requires recompilation but no migration.